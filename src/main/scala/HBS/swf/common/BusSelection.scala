package HBS.swf.common

import BaseCbb.data.GenBundle
import chisel3._
import chisel3.util.{Pipe, Valid}

class BufferOutBundle (DataW:Int,DstW:Int) extends GenBundle{
  val data = UInt(DataW.W)
  val dst  = UInt(DstW.W)
}

case class BselSample(
                     left :Boolean,
                     right:Boolean,
                     down:Boolean
                     )

class BusSelection[T<:Data](dt:T,Sample:BselSample,DstID:(Int,Int)) extends Module{
  val io = IO(new Bundle {
    val left_data_in     = Input(Valid(dt))
    val right_data_out   = Output(Valid(dt))

    val right_data_in    = Input(Valid(dt))
    val left_data_out    = Output(Valid(dt))

    val down_data_in     = Input(Valid(new BufferOutBundle(dt.getWidth,7)))
    val up_data_out      = Output(Valid(new BufferOutBundle(dt.getWidth,7)))
  })

  val left_data_in_ff  = if(Sample.left)  Pipe(io.left_data_in)  else io.left_data_in
  val right_data_in_ff = if(Sample.right) Pipe(io.right_data_in) else io.right_data_in
  val down_data_in_ff  = if(Sample.down)  Pipe(io.down_data_in)  else io.down_data_in

  io.right_data_out.valid   := left_data_in_ff.valid || (down_data_in_ff.valid && down_data_in_ff.bits.dst === DstID._2.asUInt)
  io.right_data_out.bits    := Mux(left_data_in_ff.valid, left_data_in_ff.bits,down_data_in_ff.bits.data.asTypeOf(left_data_in_ff.bits.cloneType))

  io.left_data_out.valid   := right_data_in_ff.valid || (down_data_in_ff.valid && down_data_in_ff.bits.dst === DstID._1.asUInt)
  io.left_data_out.bits    := Mux(right_data_in_ff.valid, right_data_in_ff.bits,down_data_in_ff.bits.data.asTypeOf(left_data_in_ff.bits.cloneType))

  io.up_data_out.bits  := down_data_in_ff.bits
  io.up_data_out.valid := down_data_in_ff.valid && ((down_data_in_ff.bits.dst =/= DstID._1.asUInt) || (down_data_in_ff.bits.dst =/= DstID._2.asUInt))
}





