package HBS.swf.common

import BaseCbb.data.GenModule
import chisel3._
import chisel3.util._

class BidirectionFF[T<:Data,S<:Data] (dt:T,at:S) extends GenModule{
  val io = IO(new Bundle{
    val i_a2b = Input(ValidIO(dt))
    val i_b2a = Input(ValidIO(at))
    val o_a2b = Output(ValidIO(dt))
    val o_b2a = Output(ValidIO(at))
  })

  io.o_a2b.bits  := RegEnable(io.i_a2b.bits,io.i_a2b.valid)
  io.o_a2b.valid := RegNext(io.i_a2b.valid,false.B)

  io.o_b2a.bits  := RegEnable(io.i_b2a.bits,io.i_b2a.valid)
  io.o_b2a.valid := RegNext(io.i_b2a.valid,false.B)
}


class BidirectionPipe[T<:Data,S<:Data](dt:T,at:S,Stage:Int) extends GenModule{
  val io = IO(new Bundle{
    val i_a2b = Input(ValidIO(dt))
    val i_b2a = Input(ValidIO(at))
    val o_a2b = Output(ValidIO(dt))
    val o_b2a = Output(ValidIO(at))
  })
  if(Stage==0){
    io.o_a2b := io.i_a2b
    io.o_b2a := io.i_b2a
  }else{
    val w_a2b = Wire(Vec(Stage+1,ValidIO(UInt(dt.getWidth.W))))
    val w_b2a = Wire(Vec(Stage+1,ValidIO(UInt(at.getWidth.W))))
    w_a2b(0).valid := io.i_a2b.valid
    w_a2b(0).bits  := io.i_a2b.bits.asUInt
    w_b2a(0).valid := io.i_b2a.valid
    w_b2a(0).bits  := io.i_b2a.bits.asUInt

    for(i<-0 until(Stage)){
      val U_BFF = Module(new BidirectionFF(UInt(dt.getWidth.W),UInt(at.getWidth.W))).suggestName("U_BFF_STAGE"+i.toString)
      U_BFF.io.i_a2b := w_a2b(i)
      w_a2b(i+1) := U_BFF.io.o_a2b
      U_BFF.io.i_b2a := w_b2a(i)
      w_b2a(i+1) := U_BFF.io.o_b2a
    }
    io.o_a2b := w_a2b(Stage).asTypeOf(ValidIO(dt))
    io.o_b2a := w_b2a(Stage).asTypeOf(ValidIO(at))
  }
}

object BidirectionPipe{
  def apply[T<:Data,S<:Data](a2b:ValidIO[T],b2a:ValidIO[S],stageNum:Int)={
    val U_PIPE = Module(new BidirectionPipe(a2b.bits.cloneType,b2a.bits.cloneType,stageNum))
    U_PIPE.io.i_a2b := a2b
    U_PIPE.io.i_b2a := b2a
    (U_PIPE.io.o_a2b,U_PIPE.io.o_b2a)
  }
}