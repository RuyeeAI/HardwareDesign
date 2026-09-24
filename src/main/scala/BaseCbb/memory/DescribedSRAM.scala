// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package BaseCbb.memory

import chisel3.{Data, SyncReadMem, Vec}
import chisel3.util.log2Ceil

/** 带描述信息的 SRAM：创建 `SyncReadMem`，并把名字 / 位宽 / 深度 / 描述 / 写掩码粒度
  * 在 elaboration 期打印出来（供日志与文档生成使用）。
  *
  * 由 hardware-design `BaseCbb/memory/DescribedSRAM.scala` 迁入。
  * ⚠️ chisel 7 移除了 FIRRTL 注解机制（`chisel3.experimental.ChiselAnnotation` 与
  * `firrtl.annotations`），原通过 `BaseCbb.annotation.SRAMAnnotation` 落到 .anno.json 的
  * 记录方式不复存在，改为 elaboration 期打印；`BaseCbb/annotation/` 整包随之删除。
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

    println(
      f"[DescribedSRAM] name=$name desc=$desc addrW=${log2Ceil(size)} " +
        f"dataW=${data.getWidth} depth=$size writeMaskGran=$granWidth"
    )

    mem
  }
}
