package BaseCbb

import chisel3._
import chisel3.util._


object RrLogic{
  def apply(rdy:UInt,point_ff:UInt):UInt={
    val ClientNum = rdy.getWidth
    val double_rdy = Cat(rdy, rdy)
    val double_grant = double_rdy & (~(double_rdy - point_ff))
    double_grant(ClientNum-1,0) | double_grant(2*ClientNum-1,ClientNum)
  }
}

class RR (val ClientNum:Int) extends Module{
  val io = IO(new Bundle{
    val ready = Input(UInt(ClientNum.W))
    val grant = Output(UInt(ClientNum.W))
    val enable = Input(Bool())
  })
  val point_ff = RegInit(1.U(ClientNum.W))
  when (io.enable){
    point_ff := Cat(io.grant(ClientNum-2,0),io.grant(ClientNum-1))
  }
  io.grant := RrLogic(io.ready,point_ff)
}

object RR{
  def apply(rdy:UInt,en:Bool,InstName:String = "RR"):UInt={
    val rr = Module(new RR(rdy.getWidth)).suggestName(InstName)
    rr.io.ready := rdy
    rr.io.enable := en
    rr.io.grant
  }
}

class WRR(val ClientNum:Int,WtWidth:Int) extends Module{
  val io = IO(new Bundle{
    val ready = Input(UInt(ClientNum.W))
    val grant = Output(UInt(ClientNum.W))
    val enable = Input(Bool())
    val weight = Input(Vec(ClientNum,UInt(WtWidth.W)))
  })

  val wt        = RegInit(0.U.asTypeOf(Vec(ClientNum,UInt(WtWidth.W))))
  val req       = io.ready.asTypeOf(Vec(ClientNum,Bool())).zip(wt).map(x=>x._1 && x._2>0.U)
  val load_en   = !req.reduce(_|_)
  val mask_req  = Mux(load_en,io.ready,Cat(req.reverse))
  io.grant := RR(mask_req,io.enable)
  for(i<-0 until ClientNum){
    when(io.enable){
      wt(i) := Mux(load_en, io.weight(i), wt(i)) - io.grant(i)
    }
  }
}