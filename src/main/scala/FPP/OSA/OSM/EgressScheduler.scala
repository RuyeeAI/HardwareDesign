package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Work-conserving egress scheduler (docs §3.14).
 *
 * The OSA read (osaBeat) is strict priority on the 2 x 96B egress. The two
 * loopback ports use only the leftover egress, each rate-limited by a token
 * bucket (loopRateFixed in fixed-point 8; 30/8 = 3.75 seg/c = 300 Gbps).
 * A loopback reaches its 300 Gbps cap only when the OSA read leaves enough
 * egress (network not saturated).
 */
class EgressScheduler(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val osaBeat   = Flipped(Decoupled(new CellOutputBundle(config)))
    val loop0Beat = Flipped(Decoupled(new CellOutputBundle(config)))
    val loop1Beat = Flipped(Decoupled(new CellOutputBundle(config)))
    val out       = Decoupled(new CellOutputBundle(config))
  })

  // token buckets (fixed-point 8)
  val token0 = RegInit(0.U(16.W))
  val token1 = RegInit(0.U(16.W))
  val cap = (config.loopTokenCap * 8).U   // 24 seg * 8 = 192 (fixed-point)

  token0 := Mux(token0 + config.loopRate0.U < cap, token0 + config.loopRate0.U, cap)
  token1 := Mux(token1 + config.loopRate1.U < cap, token1 + config.loopRate1.U, cap)

  // OSA strict priority
  io.out.bits := io.osaBeat.bits
  io.out.valid := io.osaBeat.valid
  io.osaBeat.ready := io.out.ready

  // loopback ports use the egress when the OSA beat is not present, each
  // gated by its token bucket (>= 8 fixed-point = 1 segment)
  val loop0Can = token0 >= 8.U && io.loop0Beat.valid
  val loop1Can = token1 >= 8.U && io.loop1Beat.valid

  val take0 = !io.osaBeat.valid && loop0Can
  val take1 = !io.osaBeat.valid && !loop0Can && loop1Can

  when(take0) {
    io.out.bits := io.loop0Beat.bits
    io.out.valid := true.B
    token0 := token0 - 8.U
  }
  when(take1) {
    io.out.bits := io.loop1Beat.bits
    io.out.valid := true.B
    token1 := token1 - 8.U
  }

  io.loop0Beat.ready := take0 && io.out.ready
  io.loop1Beat.ready := take1 && io.out.ready
}
