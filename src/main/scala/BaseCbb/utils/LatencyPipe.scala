package BaseCbb.utils

import chisel3.DontCare.:=
import chisel3._
import chisel3.util._

class LatencyPipe[T <: Data](typ: T, latency: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(typ))
    val out = Decoupled(typ)
  })

  def doN[T](n: Int, func: T => T, in: T): T =
    (0 until n).foldLeft(in)((last, _) => func(last))

  io.out <> doN(latency, (last: DecoupledIO[T]) => Queue(last, 1, true), io.in)
}
class RegEn[T<:Data](typ:T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(typ))
    val out = ValidIO(typ)
  })
  io.out.bits  := RegEnable(io.in.bits,io.in.valid)
  io.out.valid := RegNext(io.in.valid,false.B)
}
object RegEn{
  def apply[T<:Data](in:Valid[T],name:String="U_REG_EN")={
    val U_REG_EN = Module(new RegEn(in.bits.cloneType))
    U_REG_EN.io.in := in
    U_REG_EN.io.out
  }
}


class LatencyPipeV[T<:Data](typ:T,latency:Int) extends Module{
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(typ))
    val out = ValidIO(typ)
  })

  def doN(in:Valid[T]):Valid[T] = {
    RegEn(in)
  }

  io.out := (0 until latency).foldLeft(io.in)((last,_) => doN(last))
}

object LatencyPipe {
  def apply[T <: Data](in: DecoupledIO[T], latency: Int,instanceName:String="LatencyPipe"): DecoupledIO[T] = {
    val pipe = Module(new LatencyPipe(chiselTypeOf(in.bits), latency)).suggestName(instanceName)
    pipe.io.in <> in
    pipe.io.out
  }


}
object LatencyPipeV {
  def apply[T<:Data](in:Valid[T],latency:Int,instanceName:String="LatencyPipe"):Valid[T] = {
    val pipe = Module(new LatencyPipeV(chiselTypeOf(in.bits),latency)).suggestName(instanceName)
    pipe.io.in <> in
    pipe.io.out
  }
}