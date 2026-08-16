package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule
import FPP.OSA.PreParser._

/**
 * PPRS priority extraction bank (docs §3.3).
 *
 * Replicates the combinational PreParserCore datapath 3x (one per new packet
 * per cycle) while sharing a single copy of the configuration storage
 * (per-port configs, TCAM entries, VLAN/DSCP/OpaqueTag LUTs). Results are
 * pipeline-aligned by pprsLatency and delivered per slot.
 */
class PprsBank(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val in  = Vec(config.maxNewPktPerCycle, Flipped(Valid(new NewPacketWindow)))
    val out = Vec(config.maxNewPktPerCycle, Valid(new PriResult))

    // shared config storage write (CSR-style)
    val csrWriteAddr = Input(UInt(8.W))
    val csrWriteData = Input(UInt(32.W))
    val csrWriteEn   = Input(Bool())
  })

  val pprsCfg = PreParserConfig(portCount = config.pprsPorts,
                                tcamDepth = config.pprsTcamDepth)

  // ---- shared configuration storage ---------------------------------------
  val portConfigs = Reg(Vec(pprsCfg.portCount, new PortConfig))
  val tcamEntries = Reg(Vec(pprsCfg.tcamDepth, new TcamEntry))
  val vlanPrioLut = RegInit(VecInit(Seq.tabulate(128)(i => (i & 0xF).U(4.W))))
  val dscpPrioLut = RegInit(VecInit(Seq.tabulate(512)(i => (i & 0xF).U(4.W))))
  val opaquePrioLut = RegInit(VecInit(Seq.tabulate(256)(i => (i & 0xF).U(4.W))))

  // reset defaults for port configs / tcam
  for (i <- 0 until pprsCfg.portCount) {
    portConfigs(i) := PortConfig.default
    when(reset.asBool) { portConfigs(i) := PortConfig.default }
  }
  for (i <- 0 until pprsCfg.tcamDepth) {
    tcamEntries(i) := TcamEntry.default
    when(reset.asBool) { tcamEntries(i) := TcamEntry.default }
  }

  when(io.csrWriteEn && !reset.asBool) {
    val a = io.csrWriteAddr
    // 0x00-0x3F: per-port configs (4 words each)
    when(a < 0x40.U) {
      val port = (a >> 2)
      when(a(1, 0) === 0.U) {
        portConfigs(port).trustMode  := io.csrWriteData(1, 0)
        portConfigs(port).tcamEnable := io.csrWriteData(2)
        portConfigs(port).defaultPri := io.csrWriteData(6, 3)
      }
    }
    // 0x80-0xFF: TCAM entries (6 words each)
    .elsewhen(a >= 0x80.U && a < 0x100.U) {
      val idx = (a - 0x80.U) / 6.U(8.W)
      val word = (a - 0x80.U) % 6.U(8.W)
      when(word === 0.U) { tcamEntries(idx).dmacMask := io.csrWriteData(31, 0) }
      .elsewhen(word === 1.U) { tcamEntries(idx).dmacValue := io.csrWriteData(31, 0) }
      .elsewhen(word === 2.U) { tcamEntries(idx).smacMask := io.csrWriteData(31, 0) }
      .elsewhen(word === 3.U) { tcamEntries(idx).smacValue := io.csrWriteData(31, 0) }
      .elsewhen(word === 4.U) { tcamEntries(idx).priority := io.csrWriteData(3, 0) }
      .elsewhen(word === 5.U) { tcamEntries(idx).valid := io.csrWriteData(0) }
    }
    // 0x100-0x17F: VLAN LUT
    .elsewhen(a >= 0x100.U && a < 0x180.U) {
      vlanPrioLut((a - 0x100.U)(6, 0)) := io.csrWriteData(3, 0)
    }
    // 0x200-0x3FF: DSCP LUT
    .elsewhen(a >= 0x200.U && a < 0x400.U) {
      dscpPrioLut((a - 0x200.U)(8, 0)) := io.csrWriteData(3, 0)
    }
    // 0x300-0x3FF: OpaqueTag LUT
    .elsewhen(a >= 0x300.U && a < 0x400.U) {
      opaquePrioLut((a - 0x300.U)(7, 0)) := io.csrWriteData(3, 0)
    }
  }

  // ---- replicated combinational datapaths (3x) ----------------------------
  val cores = Seq.fill(config.maxNewPktPerCycle)(Module(new PreParserCore(pprsCfg)))
  for (i <- 0 until config.maxNewPktPerCycle) {
    cores(i).io.in_data   := io.in(i).bits.first32B
    cores(i).io.in_portId := io.in(i).bits.portId.pad(4)
    cores(i).io.in_valid  := io.in(i).valid
    cores(i).io.portConfig := portConfigs
    cores(i).io.tcamEntries := tcamEntries
    cores(i).io.vlanPrioLut := vlanPrioLut
    cores(i).io.dscpPrioLut := dscpPrioLut
    cores(i).io.opaquePrioLut := opaquePrioLut
  }

  // ---- pipeline alignment (pprsLatency) -----------------------------------
  // Carry {priority, portId, slotId, valid} through pprsLatency stages so the
  // priority can be matched to its packet context (docs §3.3).
  val stgValid = Seq.fill(config.pprsLatency + 1)(Wire(Vec(config.maxNewPktPerCycle, Bool())))
  val stgPort  = Seq.fill(config.pprsLatency + 1)(Wire(Vec(config.maxNewPktPerCycle, UInt(3.W))))
  val stgSlot  = Seq.fill(config.pprsLatency + 1)(Wire(Vec(config.maxNewPktPerCycle, UInt(2.W))))
  val stgPri   = Seq.fill(config.pprsLatency + 1)(Wire(Vec(config.maxNewPktPerCycle, UInt(4.W))))
  for (i <- 0 until config.maxNewPktPerCycle) {
    stgValid(0)(i) := cores(i).io.out_valid
    stgPort(0)(i)  := io.in(i).bits.portId
    stgSlot(0)(i)  := io.in(i).bits.slotId
    stgPri(0)(i)   := cores(i).io.out_priority
  }
  for (s <- 1 to config.pprsLatency) {
    for (i <- 0 until config.maxNewPktPerCycle) {
      stgValid(s)(i) := RegNext(stgValid(s - 1)(i), false.B)
      stgPort(s)(i)  := RegNext(stgPort(s - 1)(i))
      stgSlot(s)(i)  := RegNext(stgSlot(s - 1)(i))
      stgPri(s)(i)   := RegNext(stgPri(s - 1)(i))
    }
  }

  for (i <- 0 until config.maxNewPktPerCycle) {
    io.out(i).valid := stgValid(config.pprsLatency)(i)
    io.out(i).bits.portId    := stgPort(config.pprsLatency)(i)
    io.out(i).bits.slotId    := stgSlot(config.pprsLatency)(i)
    io.out(i).bits.orgQindex := stgPri(config.pprsLatency)(i)
    io.out(i).bits.src       := 0.U
    io.out(i).bits.err       := false.B
  }
}
