package BaseCbb.math

import chisel3._
import chisel3.util._

/** 16-bit internet checksum (RFC 1071 one's complement).
  *
  * Processes data words with valid/first/last handshake. Accumulates using
  * one's complement addition with carry wrap-around. Outputs ~sum on last.
  *
  * @param dataWidth width of each input word (default 16)
  */
class Checksum(dataWidth: Int = 16) extends Module {
  val io = IO(new Bundle {
    val data   = Input(UInt(dataWidth.W))
    val valid  = Input(Bool())
    val first  = Input(Bool())
    val last   = Input(Bool())
    val ready  = Output(Bool())
    val sum    = Output(UInt(16.W))
    val result = Output(UInt(16.W))
  })

  val acc       = RegInit(0.U(17.W))
  val resultReg = RegInit(0.U(16.W))

  val raw = acc + io.data
  val nextSum = WireDefault(acc)
  when(io.valid) {
    when(io.first) {
      nextSum := io.data
    }.otherwise {
      nextSum := Mux(raw(16), raw(15, 0) + 1.U, raw(15, 0))
    }
  }
  acc := nextSum

  when(io.valid && io.last) {
    resultReg := ~nextSum(15, 0)
  }

  io.ready  := true.B
  io.sum    := acc(15, 0)
  io.result := resultReg
}

object Checksum {
  def apply(data: UInt, valid: Bool, first: Bool, last: Bool): (UInt, UInt, Bool) = {
    val mod = Module(new Checksum(data.getWidth))
    mod.io.data  := data
    mod.io.valid := valid
    mod.io.first := first
    mod.io.last  := last
    (mod.io.sum, mod.io.result, mod.io.ready)
  }
}
