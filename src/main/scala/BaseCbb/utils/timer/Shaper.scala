package BaseCbb.utils.timer

import chisel3._
import chisel3.util._

/** Token bucket traffic shaper.
  *
  * Tokens accumulate at a constant rate up to a burst limit.
  * A request passes when sufficient tokens are available.
  *
  * @param tokenWidth width of token counter
  */
class Shaper(tokenWidth: Int = 16) extends Module {
  val io = IO(new Bundle {
    val rate      = Input(UInt(tokenWidth.W))
    val burstSize = Input(UInt(tokenWidth.W))
    val interval  = Input(UInt(16.W))
    val req       = Input(Bool())
    val pktSize   = Input(UInt(tokenWidth.W))
    val pass      = Output(Bool())
    val tokens    = Output(UInt(tokenWidth.W))
  })

  val tokens    = RegInit(0.U(tokenWidth.W))
  val intCnt    = RegInit(0.U(16.W))
  val interval1 = io.interval - 1.U

  when(intCnt === 0.U) {
    intCnt := interval1
    val next = tokens + io.rate
    tokens := Mux(next > io.burstSize, io.burstSize, next)
  }.otherwise {
    intCnt := intCnt - 1.U
  }

  val enough = tokens >= io.pktSize
  io.pass := io.req && enough

  when(io.req && enough) {
    tokens := tokens - io.pktSize
  }

  io.tokens := tokens
}

object Shaper {
  def apply(rate: UInt, burstSize: UInt, interval: UInt, req: Bool, pktSize: UInt): Bool = {
    val mod = Module(new Shaper(rate.getWidth))
    mod.io.rate      := rate
    mod.io.burstSize := burstSize
    mod.io.interval  := interval
    mod.io.req       := req
    mod.io.pktSize   := pktSize
    mod.io.pass
  }
}
