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
 * v2 notes: the read side is descriptor-driven — DescQueue supplies
 * {bufBase, segCount} to the read scheduler, so reads are packet-aligned and
 * per-port occupancy is decremented when segments leave the buffer (v1 used a
 * free-running read-base counter plus a global inFlight estimate, and never
 * decremented occupancy, which latched backpressure permanently).
 * Still TODO: buffer rollback on drop (AdmCtrl.rollback is counted but the
 * write pointer is not rewound), bank-conflict deferral on the write path.
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
    // 被丢弃但空间无法安全回收的报文数（地址区间后面还有别的报文，回退会踩到别人）
    val rollbackLeakCnt = Output(UInt(32.W))
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
  // 描述符的 bufBase 来自写路径锁存的每 context 首地址（必须在 wrPath 之后连接）
  adm.io.ctxStart := wrPath.io.ctxStart
  // 丢弃回退：写指针回退 + 占用释放（只回退地址区间位于尾部的那些）
  wrPath.io.rollback := adm.io.rollback

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

  // ---- read-side datapath (v2: descriptor-driven) --------------------------
  // 读地址来自描述符的 bufBase，一次读走一个报文（24 段/拍，尾拍按剩余段数截断）。
  // 描述符在 EOP + 优先级就绪后才入队，其数据必然已全部写入缓冲，因此不再需要
  // 全局 inFlight 估计去猜“有没有数据可读”——那套估计既无法归属端口，
  // 也无法给出报文边界。没有描述符时读侧静默，环回口即可占用空闲出口带宽。
  val rd = Module(new BufRdCtrl(config))
  val SEGS = config.outSegPerBeat                 // 24
  val segW = log2Ceil(SEGS + 1)                   // 5 bits (0..24)
  val segsFull = SEGS.U(segW.W)

  val curValid  = RegInit(false.B)
  val curDesc   = Reg(new PacketDesc)
  val curBase   = Reg(UInt(config.bufAddrWidth.W))
  val curRemain = Reg(UInt(16.W))
  val curFirst  = RegInit(false.B)

  val segsThisBeat = Mux(curRemain > SEGS.U, segsFull, curRemain(segW - 1, 0))

  descQ.io.deq.ready := !curValid
  when(descQ.io.deq.fire) {
    curValid  := true.B
    curDesc   := descQ.io.deq.bits
    curBase   := descQ.io.deq.bits.bufBase
    curRemain := descQ.io.deq.bits.segCount
    curFirst  := true.B
  }

  rd.io.rdBase   := curBase
  rd.io.rdEn     := curValid && io.cellOut.ready
  rd.io.segLimit := segsThisBeat
  rd.io.lastBeat := curRemain <= SEGS.U
  rd.io.wrMask   := wrPath.io.bankWe.asUInt

  when(rd.io.rdEn) {
    curBase   := curBase + SEGS.U
    curRemain := curRemain - segsThisBeat
    curFirst  := false.B
    when(curRemain <= SEGS.U) { curValid := false.B }
  }

  // 读数据比发起晚一拍，描述符与“读出量”同样打一拍对齐到数据返回时刻
  val rdPop    = RegNext(rd.io.rdEn, false.B)
  val rdDescD  = RegNext(curDesc)
  val rdFirstD = RegNext(curFirst)
  val rdSegsD  = RegNext(segsThisBeat)

  bufRam.io.rdReq := rd.io.rdReq
  rd.io.rdResp := bufRam.io.rdResp

  val cell = Module(new CellAsm(config))
  cell.io.rdData    := rd.io.rdData
  cell.io.desc.valid := rdPop
  cell.io.desc.bits := rdDescD
  cell.io.firstBeat := rdFirstD

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

  // ---- occupancy: 写入累加，段读出缓冲时按端口递减 --------------------------
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
  // 丢弃回退：只释放实际完成回退的那些（BufWrPath 判定为尾部安全）
  val rollDec = Wire(Vec(config.portCount, UInt(config.bufAddrWidth.W)))
  for (p <- 0 until config.portCount) {
    rollDec(p) := wrPath.io.rollbackApplied.map(r =>
      Mux(r.valid && r.bits.portId === p.U, r.bits.segCount, 0.U(config.bufAddrWidth.W))
    ).reduce(_ + _)
  }

  for (p <- 0 until config.portCount) {
    val inc  = cntChain(config.segmentsPerCycle)(p)
    // 读出口按端口归属递减（v1 只增不减，一旦越过门限反压就再也不会释放）
    val dec  = Mux(rdPop && rdDescD.portId === p.U, rdSegsD, 0.U(config.bufAddrWidth.W))
    val next = occ(p) + inc
    // 饱和到 0：写冲突丢段等异常下宁可少计，也不要回绕成天文数字
    val afterRd  = Mux(next >= dec, next - dec, 0.U(config.bufAddrWidth.W))
    // 丢弃回退释放（同样饱和，避免回退量超过已计数）
    occ(p) := Mux(afterRd >= rollDec(p), afterRd - rollDec(p), 0.U(config.bufAddrWidth.W))
    io.occupancy(p) := occ(p)
  }
  adm.io.occupancy := occ
  io.rollbackLeakCnt := wrPath.io.rollbackLeakCnt

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
  // descCount = 队列里的 + 正在被读调度器处理的那一个（后者仍占用着缓冲）
  for (p <- 0 until config.portCount) {
    io.descCount(p) := descQ.io.count(p) +
      Mux(curValid && curDesc.portId === p.U, 1.U, 0.U)
  }
  // drop counter: any rollback request from admission control
  val dropCnt = RegInit(0.U(16.W))
  when(adm.io.rollback.map(_.valid).reduce(_ || _)) {
    dropCnt := dropCnt + PopCount(adm.io.rollback.map(_.valid))
  }
  io.dropCnt := dropCnt
}
