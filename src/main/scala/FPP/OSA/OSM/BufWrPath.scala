package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Buffer write path (docs §3.6).
 *
 * Converts the tagged segment stream into per-bank writes:
 *   addr = port's write pointer + in-cycle count (SOP) or the context's
 *   running next address (continuation), then bank = addr mod banks,
 *   row = addr / banks.
 *
 * Position-ordered combinational chains (read the previous stage, write the
 * next stage) avoid combinational loops on dynamic Vec indexing.
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
  })

  val N = config.segmentsPerCycle
  val B = config.banks

  // per-port write pointer (absolute entry)
  val wrPtr = RegInit(VecInit(Seq.fill(config.portCount)(0.U(config.bufAddrWidth.W))))
  // per-context next write address
  val ctxNext = RegInit(VecInit(Seq.fill(config.ctxPool)(0.U(config.bufAddrWidth.W))))

  def ctxIdx(port: UInt, slot: UInt): UInt = port * config.ctxPerPort.U + slot

  // ---- per-port in-cycle write-count chain (position-ordered) --------------
  val cntChain = Seq.fill(N + 1)(Wire(Vec(config.portCount, UInt(6.W))))
  for (p <- 0 until config.portCount) cntChain(0)(p) := 0.U
  // ---- per-context next-address chain --------------------------------------
  val ctxChain = Seq.fill(N + 1)(Wire(Vec(config.ctxPool, UInt(config.bufAddrWidth.W))))
  for (c <- 0 until config.ctxPool) ctxChain(0)(c) := ctxNext(c)

  val segAddr = Wire(Vec(N, UInt(config.bufAddrWidth.W)))
  val segWe   = Wire(Vec(N, Bool()))

  for (pos <- 0 until N) {
    for (p <- 0 until config.portCount) cntChain(pos + 1)(p) := cntChain(pos)(p)
    for (c <- 0 until config.ctxPool) ctxChain(pos + 1)(c) := ctxChain(pos)(c)

    val seg = io.segs(pos)
    val port = seg.portId
    val idx = ctxIdx(port, seg.slotId)
    when(seg.valid && !seg.drop) {
      when(seg.sop) {
        segAddr(pos) := wrPtr(port) + cntChain(pos)(port)
        cntChain(pos + 1)(port) := cntChain(pos)(port) + 1.U
        ctxChain(pos + 1)(idx) := segAddr(pos) + 1.U
      }.otherwise {
        segAddr(pos) := ctxChain(pos)(idx)
        cntChain(pos + 1)(port) := cntChain(pos)(port) + 1.U
        ctxChain(pos + 1)(idx) := ctxChain(pos)(idx) + 1.U
      }
      segWe(pos) := true.B
    }.otherwise {
      segAddr(pos) := 0.U
      segWe(pos) := false.B
    }
  }

  // commit pointers
  for (p <- 0 until config.portCount) wrPtr(p) := wrPtr(p) + cntChain(N)(p)
  for (c <- 0 until config.ctxPool) ctxNext(c) := ctxChain(N)(c)

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
