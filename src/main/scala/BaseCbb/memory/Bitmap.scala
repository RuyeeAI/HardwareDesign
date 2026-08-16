package BaseCbb.memory

import BaseCbb.data.GenModule
import chisel3._
import chisel3.util._

/**
 * 寄存器内建位图分配器（小容量，直接 Reg 实现）。
 *
 * 位图语义：1 = 可用，0 = 已分配；初始全 1（全可用）。
 * 分配（req）：PriorityEncoder 选择最低可用位并清零；释放（ret）：对应位置 1。
 * 组合内核复用 memory.BitmapKernel（与 IDPool / BitmapCacheMem 语义统一）。
 */
class Bitmap(RscNum:Int) extends GenModule{
  val io = IO(new Bundle{
    val req_vld = Input(Bool())
    val req_ptr = Output(UInt(log2Ceil(RscNum).W))
    val ret_vld = Input(Bool())
    val ret_ptr = Input(UInt(log2Ceil(RscNum).W))
    val empty   = Output(Bool())
    val full    = Output(Bool())
  })

  // 位图：1 = 可用，0 = 已分配；初始全 1（全可用）—— 与 BitmapKernel 统一语义
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
