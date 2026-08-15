package BaseCbb.memory

import BaseCbb.GenModule
import chisel3._
import chisel3.util._

class Bitmap(RscNum:Int) extends GenModule{
  val io = IO(new Bundle{
    val req_vld = Input(Bool())
    val req_ptr = Output(UInt(log2Ceil(RscNum).W))
    val ret_vld = Input(Bool())
    val ret_ptr = Input(UInt(log2Ceil(RscNum).W))
    val empty   = Output(Bool())
    val full    = Output(Bool())
  })

  // 位图：1 = 空闲，0 = 已分配；初始全 1（全空闲）
  // （修复：原实现位宽误用 log2Ceil(RscNum)，且初始全 0 导致"上电即满"）
  val bitmap = RegInit(((BigInt(1) << RscNum) - 1).U(RscNum.W))

  val bitmap_set   = io.ret_vld << io.ret_ptr
  io.req_ptr := PriorityEncoder(bitmap)
  val bitmap_clear = io.req_vld << io.req_ptr
  when(io.ret_vld || io.req_vld){
    bitmap := (bitmap | bitmap_set.asUInt) & (~bitmap_clear).asUInt
  }
  io.empty := bitmap === Fill(RscNum,1.U)
  io.full  := bitmap === Fill(RscNum,0.U)
}
