package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * OSATop — top-level OSA wrapper (docs §3 / §4.2).
 *
 * Assembles the full datapath:
 *   SegDemux -> PktCtxAlloc / PprsBank -> PktAssembler -> AdmCtrl
 *   BufWrPath -> BufRam -> BufRdCtrl -> CellAsm -> EgressScheduler
 *   BpGen (backpressure to MAC); two LoopbackMemory ports feed the egress.
 *
 * v1 notes: the read side is driven by a simple read-base counter (descriptor
 * driven scheduling and bank-conflict deferral are TODO); per-port occupancy
 * is write-accumulated (read decrement TODO).
 */
class OSATop(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val mac = Flipped(new InterfaceMacOsa(config))
    val macBp = Output(new BackpressureOutput(config))
    val cellOut = Decoupled(new CellOutputBundle(config))

    // configuration (simplified CSR)
    val thresholds = Input(Vec(config.portCount, new PortThresholds))
    val bpMask     = Input(Vec(config.portCount, Vec(config.maxPfcPriority, Bool())))
    val pfcPriMap  = Input(new PfcPriMap)
    val pprsCsrAddr = Input(UInt(8.W))
    val pprsCsrData = Input(UInt(32.W))
    val pprsCsrEn   = Input(Bool())
    val lutWrAddr = Input(UInt(4.W))
    val lutWrData = Input(UInt(2.W))
    val lutWrEn   = Input(Bool())

    // loopback injection (one 32B word per cycle per port)
    val loop0WrEn  = Input(Bool())
    val loop0WrData = Input(UInt(config.loopBankWidthB.W))
    val loop1WrEn  = Input(Bool())
    val loop1WrData = Input(UInt(config.loopBankWidthB.W))

    // status
    val occupancy    = Output(Vec(config.portCount, UInt(config.bufAddrWidth.W)))
    val descCount    = Output(Vec(config.portCount, UInt(5.W)))
    val dropCnt      = Output(UInt(16.W))
    val wrConflictCnt = Output(UInt(32.W))
  })

  // ---- write-side datapath ------------------------------------------------
  val segDemux = Module(new SegDemux(config))
  segDemux.io.mac := io.mac

  val ctxAlloc = Module(new PktCtxAlloc(config))
  ctxAlloc.io.allocReq := segDemux.io.allocReq
  segDemux.io.allocGrant := ctxAlloc.io.allocGrant

  val pprs = Module(new PprsBank(config))
  for (i <- 0 until config.maxNewPktPerCycle) {
    pprs.io.in(i).valid := segDemux.io.newPktValid(i)
    pprs.io.in(i).bits  := segDemux.io.newPkt(i)
  }
  pprs.io.csrWriteAddr := io.pprsCsrAddr
  pprs.io.csrWriteData := io.pprsCsrData
  pprs.io.csrWriteEn   := io.pprsCsrEn

  val asm = Module(new PktAssembler(config))
  asm.io.segs := segDemux.io.segs
  asm.io.pri  := pprs.io.out

  val adm = Module(new AdmCtrl(config))
  adm.io.done       := asm.io.done
  adm.io.thresholds := io.thresholds

  val wrPath = Module(new BufWrPath(config))
  wrPath.io.segs := segDemux.io.segs

  val bufRam = Module(new BufRam(config))
  bufRam.io.wrReq := VecInit(wrPath.io.bankWe zip wrPath.io.bankAddr zip wrPath.io.bankData zip
                    wrPath.io.bankEop zip wrPath.io.bankBen map {
                      case ((((we, addr), data), eop), ben) =>
                        val r = Wire(new BankWrReq(config))
                        r.we := we; r.addr := addr; r.data := data; r.eop := eop; r.ben := ben
                        r
                    })

  val descQ = Module(new DescQueue(config))
  descQ.io.enq := adm.io.fwd
  descQ.io.deq.ready := false.B   // v1: descriptor-driven read scheduling is TODO

  // ---- read-side datapath (v1: simple read-base counter) -------------------
  // Reads only proceed while there is unread data (inFlight > 0), so the
  // OSA never reads un-written addresses and the loopback ports can use the
  // egress when the network has nothing to send.
  val rd = Module(new BufRdCtrl(config))
  val rdBase = RegInit(0.U(config.bufAddrWidth.W))
  val wrTotal = RegInit(0.U(32.W))
  val rdTotal = RegInit(0.U(32.W))
  // 回绕安全判据：两计数器同宽回绕取模差；只要在途段数 < 2^31 恒正确
  //（旧的 wrTotal > rdTotal 直接比较在计数器回绕后会翻转）
  val inFlight = wrTotal - rdTotal
  val rdAvail  = inFlight =/= 0.U
  rd.io.rdBase := rdBase
  rd.io.rdEn := io.cellOut.ready && rdAvail
  rd.io.wrMask := wrPath.io.bankWe.asUInt
  when(rd.io.rdEn) { rdBase := rdBase + config.outSegPerBeat.U }

  bufRam.io.rdReq := rd.io.rdReq
  rd.io.rdResp := bufRam.io.rdResp

  val cell = Module(new CellAsm(config))
  cell.io.rdData := rd.io.rdData
  cell.io.desc.valid := false.B
  cell.io.desc.bits := 0.U.asTypeOf(new PacketDesc)

  // ---- egress: strict priority OSA + work-conserving loopbacks -------------
  val loop0 = Module(new LoopbackMemory(config))
  loop0.io.wrEn := io.loop0WrEn
  loop0.io.wrData := io.loop0WrData
  loop0.io.wrAddr := 0.U
  val loop1 = Module(new LoopbackMemory(config))
  loop1.io.wrEn := io.loop1WrEn
  loop1.io.wrData := io.loop1WrData
  loop1.io.wrAddr := 0.U

  val eg = Module(new EgressScheduler(config))
  eg.io.osaBeat <> cell.io.cellOut
  // loopback beat: a 96B unit from the 32B word stream (v1: reuse the word)
  val loop0Beat = Wire(Decoupled(new CellOutputBundle(config)))
  loop0Beat.valid := loop0.io.rdEn
  loop0Beat.bits := 0.U.asTypeOf(new CellOutputBundle(config))
  loop0Beat.bits.units(0).data(0) := loop0.io.rdData(7, 0)
  loop0Beat.bits.units(0).valid(0) := true.B
  loop0Beat.ready := eg.io.loop0Beat.ready
  loop0.io.rdReady := loop0Beat.ready
  val loop1Beat = Wire(Decoupled(new CellOutputBundle(config)))
  loop1Beat.valid := loop1.io.rdEn
  loop1Beat.bits := 0.U.asTypeOf(new CellOutputBundle(config))
  loop1Beat.bits.units(0).data(0) := loop1.io.rdData(7, 0)
  loop1Beat.bits.units(0).valid(0) := true.B
  loop1Beat.ready := eg.io.loop1Beat.ready
  loop1.io.rdReady := loop1Beat.ready
  eg.io.loop0Beat <> loop0Beat
  eg.io.loop1Beat <> loop1Beat

  io.cellOut <> eg.io.out

  // ---- occupancy (v1: write-accumulated; read decrement TODO) ----------------
  val occ = RegInit(VecInit(Seq.fill(config.portCount)(0.U(config.bufAddrWidth.W))))
  // per-port write count chain (position-ordered, no combinational loop)
  val cntChain = Seq.fill(config.segmentsPerCycle + 1)(Wire(Vec(config.portCount, UInt(5.W))))
  for (p <- 0 until config.portCount) cntChain(0)(p) := 0.U
  for (pos <- 0 until config.segmentsPerCycle) {
    for (p <- 0 until config.portCount) cntChain(pos + 1)(p) := cntChain(pos)(p)
    val seg = segDemux.io.segs(pos)
    when(seg.valid && !seg.drop) {
      cntChain(pos + 1)(seg.portId) := cntChain(pos)(seg.portId) + 1.U
    }
  }
  for (p <- 0 until config.portCount) {
    occ(p) := occ(p) + cntChain(config.segmentsPerCycle)(p)
    io.occupancy(p) := occ(p)
  }
  // 每拍写入段数 = 各端口计数求和。
  //（注意不能用 Vec.asUInt —— 那是位拼接：port1 计 1 会表现为 32 而非 1）
  val wrBeat = cntChain(config.segmentsPerCycle).reduce(_ + _)

  // read/write progress counters (drive inFlight/rdAvail, wrap-safe)
  when(wrBeat =/= 0.U) { wrTotal := wrTotal + wrBeat }
  when(rd.io.rdEn)     { rdTotal := rdTotal + config.outSegPerBeat.U }
  adm.io.occupancy := occ

  // ---- backpressure ----------------------------------------------------------
  val bp = Module(new BpGen(config))
  bp.io.occupancy  := occ        // 读内部占用信号（读 Output 端口虽合法但语义不清）
  bp.io.thresholds := io.thresholds
  bp.io.pfcPriMap  := io.pfcPriMap
  bp.io.bpMask     := io.bpMask
  io.macBp := bp.io.macBp

  // context release on admission decision (forward or drop)
  val rel = Wire(Vec(config.ctxPool, Bool()))
  for (c <- 0 until config.ctxPool) rel(c) := false.B
  for (i <- 0 until config.maxNewPktPerCycle) {
    val done = asm.io.done(i)
    when(done.valid) {
      val ctx = done.bits.portId * config.ctxPerPort.U + done.bits.slotId
      rel(ctx) := true.B
    }
  }
  ctxAlloc.io.release := rel

  io.wrConflictCnt := wrPath.io.wrConflictCnt
  io.descCount := descQ.io.count
  // drop counter: any rollback request from admission control
  val dropCnt = RegInit(0.U(16.W))
  when(adm.io.rollback.map(_.valid).reduce(_ || _)) {
    dropCnt := dropCnt + PopCount(adm.io.rollback.map(_.valid))
  }
  io.dropCnt := dropCnt
}
