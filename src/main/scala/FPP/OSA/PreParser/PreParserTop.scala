package FPP.OSA.PreParser

import BaseCbb.data.{GenBundle, GenModule}
import chisel3._
import chisel3.util._

/**
 * PreParserTop - Top-level Pre-Parser module with integrated configuration and storage
 *
 * This module wraps PreParserCore and provides:
 * - Port configuration registers (trustMode, tcamEnable, defaultPri)
 * - TCAM entries (one per port)
 * - Priority LUT memories (VLAN, DSCP, OpaqueTag)
 */
class PreParserTop(
  val config: PreParserConfig = PreParserConfig()
) extends GenModule {

  val io = IO(new Bundle {
    // Input from external packet data (first 32 bytes)
    val in_data = Input(UInt(256.W))
    val in_portId = Input(UInt(4.W))
    val in_valid = Input(Bool())

    // Output
    val out_priority = Output(UInt(4.W))
    val out_valid = Output(Bool())

    // CSR configuration interface (optional debug/management)
    val csr_read_addr = Input(UInt(8.W))
    val csr_write_addr = Input(UInt(8.W))
    val csr_write_data = Input(UInt(32.W))
    val csr_write_en = Input(Bool())
    val csr_read_data = Output(UInt(32.W))
  })

  // ========== Port Configuration Registers ==========

  val portConfigs = Reg(Vec(config.portCount, new PortConfig))
  for (i <- 0 until config.portCount) {
    when(reset.asBool === false.B && io.csr_write_en) {
      val baseAddr = (i * 4).U
      when(io.csr_write_addr === baseAddr) {
        portConfigs(i).trustMode := io.csr_write_data(1, 0)
        portConfigs(i).tcamEnable := io.csr_write_data(2).asBool
        portConfigs(i).defaultPri := io.csr_write_data(6, 3)
      }
    }
  }

  // ========== TCAM Entries ==========

  val tcamEntries = Reg(Vec(config.tcamDepth, new TcamEntry))
  for (i <- 0 until config.tcamDepth) {
    when(reset.asBool === false.B && io.csr_write_en) {
      val baseAddr = 0x80.U + (i * 8).U  // TCAM entries at 0x80-0xFF
      when(io.csr_write_addr === baseAddr) {
        tcamEntries(i).dmacMask := io.csr_write_data(47, 0)
      }.elsewhen(io.csr_write_addr === baseAddr + 1.U) {
        tcamEntries(i).dmacValue := io.csr_write_data(47, 0)
      }.elsewhen(io.csr_write_addr === baseAddr + 2.U) {
        tcamEntries(i).smacMask := io.csr_write_data(47, 0)
      }.elsewhen(io.csr_write_addr === baseAddr + 3.U) {
        tcamEntries(i).smacValue := io.csr_write_data(47, 0)
      }.elsewhen(io.csr_write_addr === baseAddr + 4.U) {
        tcamEntries(i).priority := io.csr_write_data(3, 0)
      }.elsewhen(io.csr_write_addr === baseAddr + 5.U) {
        tcamEntries(i).valid := io.csr_write_data(0).asBool
      }
    }
  }

  // ========== VLAN Priority LUT ==========
  // 128 entries: {portId[3:0], vlanPrio[3:0]} -> priority[4b]
  // Initialize with pass-through: output = vlanPrio

  val vlanPrioLut = Reg(Vec(128, UInt(4.W)))
  for (i <- 0 until 128) {
    // Default: pass-through (output = input vlanPrio)
    when(reset.asBool === false.B && io.csr_write_en) {
      val addr = 0x100.U + i.U  // VLAN LUT at 0x100-0x17F
      when(io.csr_write_addr === addr) {
        vlanPrioLut(i) := io.csr_write_data(3, 0)
      }
    }.elsewhen(reset.asBool) {
      // Initialize with pass-through: priority = vlanPrio (lower 4 bits of index)
      vlanPrioLut(i) := (i & 0xF).U(4.W)
    }
  }

  // ========== DSCP Priority LUT ==========
  // 512 entries: {portId[3:0], dscp[5:0]} -> priority[4b]
  // Initialize with pass-through: output = dscp[5:2]

  val dscpPrioLut = Reg(Vec(512, UInt(4.W)))
  for (i <- 0 until 512) {
    when(reset.asBool === false.B && io.csr_write_en) {
      val addr = 0x200.U + i.U  // DSCP LUT at 0x200-0x3FF
      when(io.csr_write_addr === addr) {
        dscpPrioLut(i) := io.csr_write_data(3, 0)
      }
    }.elsewhen(reset.asBool) {
      // Initialize with pass-through: priority = dscp[5:2]
      dscpPrioLut(i) := ((i >> 6) & 0x3).U(4.W)
    }
  }

  // ========== OpaqueTag Priority LUT ==========
  // 256 entries: {portId[3:0], opaquePrio[3:0]} -> priority[4b]
  // Initialize with pass-through: output = opaquePrio

  val opaquePrioLut = Reg(Vec(256, UInt(4.W)))
  for (i <- 0 until 256) {
    when(reset.asBool === false.B && io.csr_write_en) {
      val addr = 0x400.U + i.U  // Opaque LUT at 0x400-0x4FF
      when(io.csr_write_addr === addr) {
        opaquePrioLut(i) := io.csr_write_data(3, 0)
      }
    }.elsewhen(reset.asBool) {
      // Initialize with pass-through: priority = opaquePrio
      opaquePrioLut(i) := (i & 0xF).U(4.W)
    }
  }

  // ========== CSR Read ==========

  val csrReadData = Wire(UInt(32.W))
  csrReadData := 0.U(32.W)

  // Port config read (0x00-0x3F)
  when(io.csr_read_addr < (config.portCount * 4).U) {
    val portIdx = io.csr_read_addr(5, 2)
    val cfg = portConfigs(portIdx)
    csrReadData(1, 0) := cfg.trustMode
    csrReadData(2) := cfg.tcamEnable.asUInt
    csrReadData(6, 3) := cfg.defaultPri
  }.elsewhen(io.csr_read_addr >= 0x80.U && io.csr_read_addr < 0x100.U) {
    // TCAM read (0x80-0xFF)
    val tcamIdx = (io.csr_read_addr - 0x80.U)(7, 3)
    val fieldOffset = io.csr_read_addr(2, 0)
    val entry = tcamEntries(tcamIdx)
    csrReadData := MuxLookup(fieldOffset, 0.U(32.W))(
      Seq(
        0.U -> entry.dmacMask,
        1.U -> entry.dmacValue,
        2.U -> entry.smacMask,
        3.U -> entry.smacValue,
        4.U -> Cat(28.U(28.W), entry.priority),
        5.U -> Cat(31.U(31.W), entry.valid.asUInt)
      )
    )
  }.elsewhen(io.csr_read_addr >= 0x100.U && io.csr_read_addr < 0x180.U) {
    // VLAN LUT read (0x100-0x17F)
    val lutIdx = io.csr_read_addr(6, 0)
    csrReadData(3, 0) := vlanPrioLut(lutIdx)
  }.elsewhen(io.csr_read_addr >= 0x200.U && io.csr_read_addr < 0x400.U) {
    // DSCP LUT read (0x200-0x3FF)
    val lutIdx = io.csr_read_addr(8, 0)
    csrReadData(3, 0) := dscpPrioLut(lutIdx)
  }.elsewhen(io.csr_read_addr >= 0x400.U && io.csr_read_addr < 0x500.U) {
    // Opaque LUT read (0x400-0x4FF)
    val lutIdx = io.csr_read_addr(7, 0)
    csrReadData(3, 0) := opaquePrioLut(lutIdx)
  }

  io.csr_read_data := csrReadData

  // ========== PreParserCore Instantiation ==========

  val core = Module(new PreParserCore(config))

  core.io.in_data := io.in_data
  core.io.in_portId := io.in_portId
  core.io.in_valid := io.in_valid
  core.io.portConfig := portConfigs
  core.io.tcamEntries := tcamEntries
  core.io.vlanPrioLut := vlanPrioLut
  core.io.dscpPrioLut := dscpPrioLut
  core.io.opaquePrioLut := opaquePrioLut

  io.out_priority := core.io.out_priority
  io.out_valid := core.io.out_valid
}

object PreParserTop {
  def apply(): PreParserTop = Module(new PreParserTop(PreParserConfig()))
  def apply(config: PreParserConfig): PreParserTop = Module(new PreParserTop(config))
}