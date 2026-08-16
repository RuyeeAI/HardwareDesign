package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Per-port packet descriptor queue (docs §3.8).
 *
 * Holds committed PacketDesc of admitted packets per port, FIFO order.
 * Register-based shallow FIFO (depth 16) for this implementation; the full
 * design uses SyncFifo with external SRAM.
 */
class DescQueue(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val enq   = Flipped(Vec(config.maxNewPktPerCycle, Valid(new PacketDesc)))
    val deq   = Decoupled(new PacketDesc)
    val count = Output(Vec(config.portCount, UInt(5.W)))
  })

  val depth = 16
  val head = RegInit(VecInit(Seq.fill(config.portCount)(0.U(4.W))))
  val tail = RegInit(VecInit(Seq.fill(config.portCount)(0.U(4.W))))
  val cnt  = RegInit(VecInit(Seq.fill(config.portCount)(0.U(5.W))))
  val mem  = Reg(Vec(config.portCount, Vec(depth, new PacketDesc)))

  // enqueue (any port), at most 3 per cycle
  for (i <- 0 until config.maxNewPktPerCycle) {
    val p = io.enq(i).bits.portId
    val room = cnt(p) < depth.U
    when(io.enq(i).valid && room) {
      mem(p)(tail(p)) := io.enq(i).bits
      tail(p) := tail(p) + 1.U
      cnt(p) := cnt(p) + 1.U
    }
  }

  // dequeue: round-robin over ports with data (simplified: port 0 first)
  val selPort = RegInit(0.U(3.W))
  val hasData = cnt(selPort) > 0.U
  io.deq.valid := hasData && io.deq.ready
  io.deq.bits := mem(selPort)(head(selPort))
  when(io.deq.fire()) {
    head(selPort) := head(selPort) + 1.U
    cnt(selPort) := cnt(selPort) - 1.U
    selPort := Mux(selPort === (config.portCount - 1).U, 0.U, selPort + 1.U)
  }

  for (p <- 0 until config.portCount) io.count(p) := cnt(p)
}
