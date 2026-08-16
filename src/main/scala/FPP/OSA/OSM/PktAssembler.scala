package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Packet assembler (docs §3.5).
 *
 * Tracks the up-to-24 in-flight packet contexts: captures the 8B MAC header
 * at SOP, accumulates byte/segment counts, latches EOP/err, and aligns the
 * PPRS priority (delayed by pprsLatency) to its context. When a context has
 * both EOP and priority, it publishes an assembly-complete event; the
 * admission controller (AdmCtrl) then decides forward/drop and releases the
 * context.
 *
 * Segments tagged `drop` (ctx-full / SOP-overflow) never reach the buffer.
 */
class PktAssembler(config: OSAConfig) extends GenModule {

  class CtxState extends Bundle {
    val busy     = Bool()
    val macHeader = UInt(64.W)
    val byteCount = UInt(16.W)
    val segCount  = UInt(16.W)
    val eopSeen  = Bool()
    val err      = Bool()
    val priValid = Bool()
    val priClass = UInt(2.W)
    val orgQindex = UInt(4.W)
  }

  val io = IO(new Bundle {
    val segs = Flipped(Vec(config.segmentsPerCycle, new TaggedSeg))
    val pri  = Flipped(Vec(config.maxNewPktPerCycle, Valid(new PriResult)))

    // one completion event per context that finished this cycle (<= 3)
    val done = Output(Vec(config.maxNewPktPerCycle, Valid(new PktAssemblyDone)))
  })

  val ctxW = log2Ceil(config.ctxPool)
  val N = config.segmentsPerCycle

  // ---- per-context state ---------------------------------------------------
  val state = RegInit(VecInit(Seq.fill(config.ctxPool)(0.U.asTypeOf(new CtxState))))

  // running state chain: apply segment updates then priority updates in order
  val run = Seq.fill(N + config.maxNewPktPerCycle + 1)(Wire(Vec(config.ctxPool, new CtxState)))
  for (c <- 0 until config.ctxPool) run(0)(c) := state(c)

  def ctxIndex(port: UInt, slot: UInt): UInt = port * config.ctxPerPort.U + slot

  // ---- segment updates ------------------------------------------------------
  for (p <- 0 until N) {
    val seg = io.segs(p)
    val idx = ctxIndex(seg.portId, seg.slotId)
    for (c <- 0 until config.ctxPool) run(p + 1)(c) := run(p)(c)
    when(seg.valid && !seg.drop) {
      val st = run(p)(idx)
      val nxt = Wire(new CtxState)
      nxt := st
      nxt.busy := true.B
      when(seg.sop) {
        nxt.macHeader := seg.data          // first 8B = MAC header (TS + reserved)
        nxt.byteCount := 8.U
        nxt.segCount := 1.U
        nxt.eopSeen := seg.eop
        nxt.err := seg.err
      }.otherwise {
        nxt.byteCount := st.byteCount + 8.U
        nxt.segCount := st.segCount + 1.U
        nxt.eopSeen := st.eopSeen || seg.eop
        nxt.err := st.err || seg.err
      }
      run(p + 1)(idx) := nxt
    }
  }

  // ---- priority updates (from PprsBank, already latency-aligned) ------------
  // map orgQindex -> priClass via the PriMapper LUT
  val priMapper = Module(new PriMapper)
  priMapper.io.lutWrAddr := 0.U
  priMapper.io.lutWrData := 0.U
  priMapper.io.lutWrEn   := false.B

  // PriMapper is combinational; drive its input unconditionally (v1: first
  // valid lane; the per-lane mapping is identical since the LUT is shared)
  priMapper.io.orgQindex := Mux(io.pri(0).valid, io.pri(0).bits.orgQindex, 0.U)
  for (i <- 0 until config.maxNewPktPerCycle) {
    val base = N + i
    for (c <- 0 until config.ctxPool) run(base + 1)(c) := run(base)(c)
    when(io.pri(i).valid) {
      val idx = ctxIndex(io.pri(i).bits.portId, io.pri(i).bits.slotId)
      val st = run(base)(idx)
      val nxt = Wire(new CtxState)
      nxt := st
      nxt.priValid := true.B
      nxt.orgQindex := io.pri(i).bits.orgQindex
      nxt.priClass := priMapper.io.priClass
      run(base + 1)(idx) := nxt
    }
  }

  // ---- completion detection ------------------------------------------------
  // a context with busy && eopSeen && priValid is complete; up to 3 per cycle
  val complete = Wire(Vec(config.ctxPool, Bool()))
  for (c <- 0 until config.ctxPool) {
    val st = run(N + config.maxNewPktPerCycle)(c)
    complete(c) := st.busy && st.eopSeen && st.priValid
  }
  val compRank = Wire(Vec(config.ctxPool, UInt(3.W)))
  // rank of each complete ctx among complete ones (prefix-sum chain)
  val rankAcc = Wire(Vec(config.ctxPool + 1, UInt(3.W)))
  rankAcc(0) := 0.U
  for (c <- 0 until config.ctxPool) {
    compRank(c) := rankAcc(c)
    rankAcc(c + 1) := Mux(complete(c), rankAcc(c) + 1.U, rankAcc(c))
  }

  for (i <- 0 until config.maxNewPktPerCycle) {
    val isIth = Wire(Vec(config.ctxPool, Bool()))
    for (c <- 0 until config.ctxPool) isIth(c) := complete(c) && compRank(c) === i.U
    io.done(i).valid := isIth.reduce(_ || _)
    val st = Mux1H(isIth, VecInit((0 until config.ctxPool).map(c => run(N + config.maxNewPktPerCycle)(c))))
    val port = Mux1H(isIth, VecInit((0 until config.ctxPool).map(c => (c / config.ctxPerPort).U(3.W))))
    val slot = Mux1H(isIth, VecInit((0 until config.ctxPool).map(c => (c % config.ctxPerPort).U(2.W))))
    io.done(i).bits.portId    := port
    io.done(i).bits.slotId    := slot
    io.done(i).bits.macHeader := st.macHeader
    io.done(i).bits.byteCount := st.byteCount
    io.done(i).bits.segCount  := st.segCount
    io.done(i).bits.orgQindex := st.orgQindex
    io.done(i).bits.priClass  := st.priClass
    io.done(i).bits.err       := st.err
    io.done(i).bits.tooSmall  := st.byteCount < config.minPktSize.U
  }

  // ---- register update -------------------------------------------------------
  for (c <- 0 until config.ctxPool) {
    // a completed context clears its busy flag (the slot becomes free for
    // re-allocation next cycle; the release to PktCtxAlloc is done by AdmCtrl)
    val fin = run(N + config.maxNewPktPerCycle)(c)
    val nxt = Wire(new CtxState)
    nxt := fin
    when(complete(c)) { nxt.busy := false.B }
    state(c) := nxt
  }
}
