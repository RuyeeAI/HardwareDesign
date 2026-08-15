// See LICENSE.SiFive for license details.

package BaseCbb.memory

import chisel3._
import chisel3.util._

class IDPool(numIds: Int, lateValid: Boolean = false, revocableSelect: Boolean = false) extends Module {
  require (numIds > 0)
  val idWidth = log2Up(numIds)

  val io = IO(new Bundle {
    val free = Flipped(Valid(UInt(idWidth.W)))
    val alloc = if (revocableSelect) Decoupled(UInt(idWidth.W)) else Irrevocable(UInt(idWidth.W))
  })

  // True indicates that the id is available（1 = 可用，与 BitmapKernel 统一语义）
  val bitmap = RegInit(UInt(numIds.W), -1.S(numIds.W).asUInt)
  val select = RegInit(0.U(idWidth.W))
  val valid  = RegInit(true.B)

  io.alloc.valid := (if (lateValid) BitmapKernel.hasFree(bitmap) else valid)
  io.alloc.bits  := (if (revocableSelect) BitmapKernel.firstFree(bitmap) else select)

  // 分配（清 0）/ 释放（置 1），共享 BitmapKernel 组合内核
  val b_alloc  = Mux(io.alloc.ready, BitmapKernel.allocUpdate(bitmap, io.alloc.bits), bitmap)
  val bitmap1  = Mux(io.free.valid,  BitmapKernel.freeUpdate(b_alloc, io.free.bits), b_alloc)
  val select1  = BitmapKernel.firstFree(bitmap1)
  val valid1   = (BitmapKernel.hasFree(bitmap) &&
                  !((BitmapKernel.freeCount(bitmap) === 1.U) && io.alloc.ready)) || io.free.valid

  // Clock gate the bitmap
  when (io.alloc.ready || io.free.valid) {
    bitmap := bitmap1
    valid  := valid1
  }

  // Make select irrevocable
  when (io.alloc.ready || (!io.alloc.valid && io.free.valid)) {
    select := select1
  }

  // No double freeing（taken = 本拍正在被分配的位）
  val taken = Mux(io.alloc.ready, UIntToOH(io.alloc.bits, numIds), 0.U)
  assert (!io.free.valid || !(bitmap & ~taken)(io.free.bits))

  // pre-calculations for timing
  if (!lateValid) {
    assert (valid === BitmapKernel.hasFree(bitmap))
  }
  if (!revocableSelect) {
    when (io.alloc.valid && RegNext(io.alloc.ready || (!io.alloc.valid && io.free.valid))) {
      assert (select === BitmapKernel.firstFree(bitmap))
    }
  }
}
