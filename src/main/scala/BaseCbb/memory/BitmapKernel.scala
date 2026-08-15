package BaseCbb.memory

import chisel3._
import chisel3.util.{PopCount, PriorityEncoder, UIntToOH}

/**
 * 位图资源分配内核（纯组合函数，位图语义统一为 **1 = 可用**）。
 *
 * 供 Bitmap（memory）、IDPool、BitmapCacheMem 共享，消除三处重复的
 * "PriorityEncoder + 置位/清位"实现，并统一 empty/full 语义：
 *  - empty = 全可用（无已分配资源）
 *  - full  = 全占（无可用资源）
 */
object BitmapKernel {

  /** 最低可用位索引（无可用位时结果无意义，先判断 hasFree/isFull） */
  def firstFree(bitmap: UInt): UInt = PriorityEncoder(bitmap)

  /** 是否存在可用位 */
  def hasFree(bitmap: UInt): Bool = bitmap.orR

  /** 全可用（池空：无已分配资源） */
  def isEmpty(bitmap: UInt): Bool = bitmap.andR

  /** 全占（无可用资源） */
  def isFull(bitmap: UInt): Bool = !bitmap.orR

  /** 占用 idx（清 0） */
  def allocUpdate(bitmap: UInt, idx: UInt): UInt =
    bitmap & ~UIntToOH(idx, bitmap.getWidth)

  /** 释放 idx（置 1） */
  def freeUpdate(bitmap: UInt, idx: UInt): UInt =
    bitmap | UIntToOH(idx, bitmap.getWidth)

  /** 可用位数量 */
  def freeCount(bitmap: UInt): UInt = PopCount(bitmap)
}
