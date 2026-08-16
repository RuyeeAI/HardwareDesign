package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Packet context slot allocator (docs §3.2).
 *
 * Manages the packet-context pool: portCount x ctxPerPort slots. Allocation
 * is position-ordered (the caller presents up to maxNewPktPerCycle requests
 * in stream order; a slot released by a same-cycle EOP is visible to later
 * requests). Slots are released by the admission decision (both forward and
 * drop paths).
 */
class PktCtxAlloc(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    // up to maxNewPktPerCycle allocation requests, in stream order
    val allocReq   = Input(Vec(config.maxNewPktPerCycle, Valid(UInt(log2Ceil(config.portCount).W))))
    val allocGrant = Output(Vec(config.maxNewPktPerCycle, Valid(UInt(log2Ceil(config.ctxPerPort).W))))
    // per-context release (flat ctxId = port * ctxPerPort + slot)
    val release    = Input(Vec(config.ctxPool, Bool()))
    val busy       = Output(Vec(config.ctxPool, Bool()))
  })

  val slotW = log2Ceil(config.ctxPerPort)          // 2 bits (0..2)

  // per-port busy bitmaps: bit s = context slot s of that port is in flight
  val busyMap = RegInit(VecInit(Seq.fill(config.portCount)(0.U(config.ctxPerPort.W))))

  // combinational per-port helpers
  def firstFree(m: UInt): UInt = {
    val inv = ~m
    Mux(inv(0), 0.U, Mux(inv(1), 1.U, 2.U))
  }
  def hasFree(m: UInt): Bool = !m.andR

  // running busy map across the up-to-3 position-ordered requests:
  // request i sees the map updated by requests 0..i-1 (same-cycle EOP
  // releases are NOT visible mid-cycle for alloc — the caller releases on
  // admission decision, not at EOP, so this is consistent).
  val running = Wire(Vec(config.maxNewPktPerCycle + 1, Vec(config.portCount, UInt(config.ctxPerPort.W))))
  for (p <- 0 until config.portCount) running(0)(p) := busyMap(p)
  for (i <- 0 until config.maxNewPktPerCycle) {
    val p = io.allocReq(i).bits
    val g = io.allocReq(i).valid && hasFree(running(i)(p))
    io.allocGrant(i).valid := g
    io.allocGrant(i).bits  := firstFree(running(i)(p))
    for (p2 <- 0 until config.portCount) {
      running(i + 1)(p2) := Mux(
        g && p2.U === p,
        running(i)(p2) | UIntToOH(io.allocGrant(i).bits, config.ctxPerPort),
        running(i)(p2))
    }
  }

  // next state: last running map, then clear released contexts
  val anyAlloc = io.allocReq.map(_.valid).reduce(_ || _)
  val anyRel   = io.release.reduce(_ || _)
  val next = Wire(Vec(config.portCount, UInt(config.ctxPerPort.W)))
  for (p <- 0 until config.portCount) {
    // release chain (avoid self-reference in a combinational loop)
    val clr = Wire(Vec(config.ctxPerPort + 1, UInt(config.ctxPerPort.W)))
    clr(0) := running(config.maxNewPktPerCycle)(p)
    for (s <- 0 until config.ctxPerPort) {
      clr(s + 1) := Mux(io.release(p * config.ctxPerPort + s),
                        clr(s) & ~UIntToOH(s.U, config.ctxPerPort), clr(s))
    }
    next(p) := clr(config.ctxPerPort)
  }
  when(anyAlloc || anyRel) {
    for (p <- 0 until config.portCount) busyMap(p) := next(p)
  }

  for (p <- 0 until config.portCount) {
    for (s <- 0 until config.ctxPerPort) {
      io.busy(p * config.ctxPerPort + s) := busyMap(p)(s)
    }
  }
}
