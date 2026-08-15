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

  // 位图：1 = 可用，0 = 已分配；初始全 1（全可用）—— 与 utils.data.BitmapKernel 统一语义
  // （修复：原实现位宽误用 log2Ceil(RscNum)，且初始全 0 导致"上电即满"）
  val bitmap = RegInit(((BigInt(1) << RscNum) - 1).U(RscNum.W))

  io.req_ptr := BitmapKernel.firstFree(bitmap)
  val set = Mux(io.ret_vld, UIntToOH(io.ret_ptr, RscNum), 0.U) // 释放置 1
  val clr = Mux(io.req_vld, UIntToOH(io.req_ptr, RscNum), 0.U) // 分配清 0
  when(io.ret_vld || io.req_vld){
    bitmap := (bitmap | set) & ~clr
  }
  io.empty := BitmapKernel.isEmpty(bitmap)
  io.full  := BitmapKernel.isFull(bitmap)
}
