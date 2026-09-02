package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Buffer write path (docs §3.6).
 *
 * Converts the tagged segment stream into per-bank writes:
 *   addr = write pointer + in-cycle count (SOP) or the context's running next
 *   address (continuation), then bank = addr mod banks, row = addr / banks.
 *
 * Position-ordered combinational chains (read the previous stage, write the
 * next stage) avoid combinational loops on dynamic Vec indexing.
 *
 * 修复记录：
 *  - 写指针由「每端口各一个、全部从 0 开始」改为**全局单一指针**。BUFFER 是全端口
 *    共享的同一地址空间（docs §B.5），每端口独立指针会让 8 个端口写进同一批地址
 *    互相覆盖。每端口的门限/占用仍按端口分别统计，只是不再预留固定分区。
 *  - 新增丢弃回退：AdmCtrl 判定丢弃时回退写指针。报文段在端口内可能被其它报文
 *    穿插，因此只有在被丢报文正好位于写指针尾部时回退才是安全的（其地址区间
 *    后面没有别的报文），此时空间可被复用并同步释放占用；否则记为泄漏
 *    （`rollbackLeakCnt`），宁可少回收也不能把还在用的地址分配出去。
 *
 * NOTE (v1): multi-port interleaving can map two segments to the same bank
 * in one cycle; the full design resolves this with a write-priority arbiter
 * + one-cycle defer. This implementation drops the lower-priority conflict
 * and counts it (`wrConflictCnt`).
 */
class BufWrPath(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val segs    = Flipped(Vec(config.segmentsPerCycle, new TaggedSeg))
    val bankWe  = Output(Vec(config.banks, Bool()))
    val bankAddr= Output(Vec(config.banks, UInt(config.bankRowAddrW.W)))
    val bankData= Output(Vec(config.banks, UInt(64.W)))
    val bankEop = Output(Vec(config.banks, Bool()))
    val bankBen = Output(Vec(config.banks, UInt(8.W)))
    val wrConflictCnt = Output(UInt(32.W))
    // 每个 context 当前报文的首段缓冲地址（供 AdmCtrl 填 PacketDesc.bufBase）
    val ctxStart = Output(Vec(config.ctxPool, UInt(config.bufAddrWidth.W)))
    // 丢弃回退请求（来自 AdmCtrl）
    val rollback = Flipped(Vec(config.maxNewPktPerCycle, Valid(new RollbackInfo)))
    // 实际完成回退的丢弃（仅尾部安全的那些），供 OSATop 释放占用计数
    val rollbackApplied = Output(Vec(config.maxNewPktPerCycle, Valid(new RollbackInfo)))
    // 因不在尾部而无法安全回收的丢弃段数累计
    val rollbackLeakCnt = Output(UInt(32.W))
  })

  val N = config.segmentsPerCycle
  val B = config.banks
  val K = config.maxNewPktPerCycle

  // 全局写指针：所有端口共享同一个缓冲池
  val wrPtr = RegInit(0.U(config.bufAddrWidth.W))
  // per-context next write address
  val ctxNext = RegInit(VecInit(Seq.fill(config.ctxPool)(0.U(config.bufAddrWidth.W))))
  // per-context 当前报文首地址（SOP 段所在地址）
  val ctxStartReg = RegInit(VecInit(Seq.fill(config.ctxPool)(0.U(config.bufAddrWidth.W))))

  def ctxIdx(port: UInt, slot: UInt): UInt = port * config.ctxPerPort.U + slot

  // ---- in-cycle write-count chain (position-ordered) -----------------------
  val cntChain = Seq.fill(N + 1)(Wire(UInt(log2Ceil(N + 1).W)))
  cntChain(0) := 0.U
  // ---- per-context next-address chain --------------------------------------
  val ctxChain = Seq.fill(N + 1)(Wire(Vec(config.ctxPool, UInt(config.bufAddrWidth.W))))
  for (c <- 0 until config.ctxPool) ctxChain(0)(c) := ctxNext(c)
  // 首地址链：SOP 段写入时锁存，其余位置透传
  val startChain = Seq.fill(N + 1)(Wire(Vec(config.ctxPool, UInt(config.bufAddrWidth.W))))
  for (c <- 0 until config.ctxPool) startChain(0)(c) := ctxStartReg(c)

  val segAddr = Wire(Vec(N, UInt(config.bufAddrWidth.W)))
  val segWe   = Wire(Vec(N, Bool()))

  for (pos <- 0 until N) {
    cntChain(pos + 1) := cntChain(pos)
    for (c <- 0 until config.ctxPool) ctxChain(pos + 1)(c) := ctxChain(pos)(c)
    for (c <- 0 until config.ctxPool) startChain(pos + 1)(c) := startChain(pos)(c)

    val seg = io.segs(pos)
    val idx = ctxIdx(seg.portId, seg.slotId)
    when(seg.valid && !seg.drop) {
      when(seg.sop) {
        segAddr(pos) := wrPtr + cntChain(pos)
        ctxChain(pos + 1)(idx) := segAddr(pos) + 1.U
        startChain(pos + 1)(idx) := segAddr(pos)
      }.otherwise {
        segAddr(pos) := ctxChain(pos)(idx)
        ctxChain(pos + 1)(idx) := ctxChain(pos)(idx) + 1.U
      }
      cntChain(pos + 1) := cntChain(pos) + 1.U
      segWe(pos) := true.B
    }.otherwise {
      segAddr(pos) := 0.U
      segWe(pos) := false.B
    }
  }

  // ---- commit pointers + 丢弃回退 -------------------------------------------
  val wrPtrNext = wrPtr + cntChain(N)
  val rollDecChain = Seq.fill(K + 1)(Wire(UInt(config.bufAddrWidth.W)))
  rollDecChain(0) := 0.U
  for (i <- 0 until K) {
    rollDecChain(i + 1) := rollDecChain(i)
    val rb  = io.rollback(i)
    val ctx = ctxIdx(rb.bits.portId, rb.bits.slotId)
    // 尾部安全判据：该 context 的下一段地址正好等于本拍结束后的写指针
    val atTail = rb.valid && ctxChain(N)(ctx) === wrPtrNext
    io.rollbackApplied(i).valid := atTail
    io.rollbackApplied(i).bits  := rb.bits
    when(atTail) { rollDecChain(i + 1) := rollDecChain(i) + rb.bits.segCount }
  }
  // 至多一个 context 能满足 atTail（地址唯一），回退后不会越界
  wrPtr := wrPtrNext - rollDecChain(K)

  val leakCnt = RegInit(0.U(32.W))
  val leaked = Wire(UInt(log2Ceil(K + 1).W))
  leaked := PopCount(VecInit((0 until K).map(i => io.rollback(i).valid && !io.rollbackApplied(i).valid)))
  when(leaked =/= 0.U) { leakCnt := leakCnt + leaked }
  io.rollbackLeakCnt := leakCnt

  for (c <- 0 until config.ctxPool) ctxNext(c) := ctxChain(N)(c)
  for (c <- 0 until config.ctxPool) ctxStartReg(c) := startChain(N)(c)
  io.ctxStart := ctxStartReg

  // ---- bank mapping + conflict detection ------------------------------------
  val bankSel = Wire(Vec(N, UInt(log2Ceil(B).W)))
  val rowSel  = Wire(Vec(N, UInt(config.bankRowAddrW.W)))
  for (p <- 0 until N) {
    bankSel(p) := segAddr(p) % B.U(config.bufAddrWidth.W)
    rowSel(p)  := (segAddr(p) / B.U(config.bufAddrWidth.W))(config.bankRowAddrW - 1, 0)
  }

  val dropMask = Wire(Vec(N, Bool()))
  val takenChain = Seq.fill(N + 1)(Wire(Vec(B, Bool())))
  for (b <- 0 until B) takenChain(0)(b) := false.B
  for (p <- 0 until N) {
    for (b <- 0 until B) takenChain(p + 1)(b) := takenChain(p)(b)
    val b = bankSel(p)
    val conflict = segWe(p) && takenChain(p)(b)
    dropMask(p) := conflict
    when(segWe(p) && !conflict) { takenChain(p + 1)(b) := true.B }
  }

  val wrConflictCnt = RegInit(0.U(32.W))
  when(PopCount(dropMask) > 0.U) { wrConflictCnt := wrConflictCnt + PopCount(dropMask) }
  io.wrConflictCnt := wrConflictCnt

  for (b <- 0 until B) {
    io.bankWe(b) := false.B
    io.bankAddr(b) := 0.U
    io.bankData(b) := 0.U
    io.bankEop(b) := false.B
    io.bankBen(b) := 0.U
  }
  for (p <- 0 until N) {
    val b = bankSel(p)
    when(segWe(p) && !dropMask(p)) {
      io.bankWe(b) := true.B
      io.bankAddr(b) := rowSel(p)
      io.bankData(b) := io.segs(p).data
      io.bankEop(b) := io.segs(p).eop
      io.bankBen(b) := io.segs(p).byteEn
    }
  }
}
