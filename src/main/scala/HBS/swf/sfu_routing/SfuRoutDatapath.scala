package HBS.swf.sfu_routing

import BaseCbb.data.GenModule
import BaseCbb.misc.LatencyPipeV
import chisel3._
import chisel3.util.ValidIO

class SfuDatapath [T<:Data] (dt:T,Latency:Seq[Int]) extends GenModule{
  val io = IO(new Bundle{
    val ib_in  = Flipped(ValidIO(dt))
    val up_out = ValidIO(dt)
    val dn_out = ValidIO(dt)
  })

  val U_IN_PIPE = Module(new LatencyPipeV(UInt(dt.getWidth.W),Latency(0)))
  val U_UP_PIPE = Module(new LatencyPipeV(UInt(dt.getWidth.W),Latency(1)))
  val U_DN_PIPE = Module(new LatencyPipeV(UInt(dt.getWidth.W),Latency(2)))

  U_IN_PIPE.io.in.valid := io.ib_in.valid
  U_IN_PIPE.io.in.bits  := io.ib_in.bits.asUInt
  U_UP_PIPE.io.in       := U_IN_PIPE.io.out
  U_DN_PIPE.io.in       := U_IN_PIPE.io.out
  io.up_out.bits        := U_UP_PIPE.io.out.bits.asTypeOf(dt.cloneType)
  io.up_out.valid       := U_UP_PIPE.io.out.valid
  io.dn_out.bits        := U_DN_PIPE.io.out.bits.asTypeOf(dt.cloneType)
  io.dn_out.valid       := U_DN_PIPE.io.out.valid
}
