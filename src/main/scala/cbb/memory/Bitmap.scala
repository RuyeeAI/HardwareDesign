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

  val bitmap = RegInit(0.U(log2Ceil(RscNum).W))

  val bitmap_set   = io.ret_vld << io.ret_ptr
  io.req_ptr := PriorityEncoder(bitmap)
  val bitmap_clear = io.req_vld << io.req_ptr
  when(io.ret_vld || io.req_vld){
    bitmap := (bitmap | bitmap_set.asUInt) & (~bitmap_clear).asUInt
  }
  io.empty := bitmap === Fill(RscNum,1.U)
  io.full  := bitmap === Fill(RscNum,0.U)
}
