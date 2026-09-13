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
  // 仅在实际发出授权时旋转指针。
  // 修复：enable=1 且无人 ready 时 grant=0，旧实现会把 point_ff 写成 0，
  // 而 RrLogic(rdy, 0) ≡ 0 —— 仲裁器从此永久卡死（WRR 经内部 RR 同样暴露）。
  when (io.enable && io.grant.orR) {
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
  // 修复：带饱和的扣减。旧实现 `next - grant(i)` 在 weight=0 却被授予（load 拍 mask_req
  // 不看权重）或 wt 恰好扣到 0 的拍会回绕成巨值，客户端从此霸占仲裁。
  for(i<-0 until ClientNum){
    when(io.enable){
      val next  = Mux(load_en, io.weight(i), wt(i))
      val spent = Mux(io.grant(i), 1.U(WtWidth.W), 0.U(WtWidth.W))
      wt(i) := Mux(next > spent, next - spent, 0.U(WtWidth.W))
    }
  }
}