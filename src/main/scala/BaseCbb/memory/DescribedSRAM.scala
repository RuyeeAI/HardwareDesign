// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package BaseCbb.memory

import chisel3.{Data, SyncReadMem, Vec}
import chisel3.util.log2Ceil
import BaseCbb.annotation.Annotated

/** 带描述信息的 SRAM：创建 `SyncReadMem` 并通过 `BaseCbb.annotation.SRAMAnnotation`
  * 把名字 / 位宽 / 深度 / 描述 / 写掩码粒度记录进 annotation（供后端与文档生成使用）。
  *
  * 由 hardware-design `BaseCbb/memory/DescribedSRAM.scala` 迁入，仅把
  * `BaseCbb.utils.Annotated`（HBS 时期路径）改为 HD 的 `BaseCbb.annotation.Annotated`。
  */
object DescribedSRAM {
  def apply[T <: Data](
    name: String,
    desc: String,
    size: BigInt, // depth
    data: T
  ): SyncReadMem[T] = {

    val mem = SyncReadMem(size, data)

    mem.suggestName(name)

    val granWidth = data match {
      case v: Vec[_] => v.head.getWidth
      case d         => d.getWidth
    }

    Annotated.srams(
      component = mem,
      name = name,
      address_width = log2Ceil(size),
      data_width = data.getWidth,
      depth = size,
      description = desc,
      write_mask_granularity = granWidth
    )

    mem
  }
}
