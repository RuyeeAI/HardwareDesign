package BaseCbb.RegCbb_v2

import scala.collection.mutable.ArrayBuffer

/** 字段在寄存器内的位分配 */
case class FieldAllocation(field: RegFieldDef, bitOffset: Int) {
  def hi: Int = bitOffset + field.bitWidth - 1
}

/** 寄存器分配（含字节偏移与字段位偏移） */
case class RegAllocation(
  reg: RegDef,
  byteOffset: BigInt,
  fieldAllocations: Seq[FieldAllocation]
) {
  def totalBits: Int = reg.totalBits
  def wordCount: Int = reg.wordCount
  def byteSize: Int = reg.byteSize
}

/** 存储器分配 */
case class MemAllocation(mem: MemoryDef, baseAddress: BigInt)

/** 完整地址映射：供 RTL / 文档 / 软件视图共同使用 */
case class RegFileMap(
  block: RegBlockDef,
  regs: Seq[RegAllocation],
  mems: Seq[MemAllocation]
) {
  def totalRegByteSize: BigInt = if (regs.isEmpty) 0 else regs.map(r => r.byteOffset + r.byteSize).max
  def totalMemByteSize: BigInt = if (mems.isEmpty) 0 else mems.map(m => m.baseAddress + m.mem.byteSize).max
  def regByName(name: String): RegAllocation =
    regs.find(_.reg.name == name).getOrElse(sys.error(s"register '$name' not found"))
}

/**
 * 地址分配器：
 *  - 字段按 LSB-first 紧凑排列；
 *  - 寄存器按 32bit word 对齐（wordCount = ceil(totalBits/32)，byteSize = wordCount*4），
 *    保证任何寄存器都不会跨越其所属的 word 边界；
 *  - 存储器地址自动分配：从块 memBaseAddress 起，按 dataWidth/8 字节自然对齐顺序分配；
 *    用户也可在定义时手工指定 baseAddress（自动分配会跳过已占用的区域）。
 */
object AddressAllocator {

  def allocate(block: RegBlockDef): RegFileMap = {
    var off = BigInt(0)
    val regs = block.registers.map { r =>
      var bit = 0
      val fas = r.fields.map { f =>
        val fa = FieldAllocation(f, bit)
        bit += f.bitWidth
        fa
      }
      val ra = RegAllocation(r, off, fas)
      off += r.byteSize
      ra
    }

    // 存储器地址：自动分配（None）或手工指定（Some）
    var memOff = block.memBaseAddress
    val mems = block.memories.map { m =>
      val align = BigInt(m.dataWidth / 8)
      val base = m.baseAddress match {
        case Some(a) => a
        case None    => ((memOff + align - 1) / align) * align // 自然对齐
      }
      if (base + m.byteSize > memOff) memOff = base + m.byteSize // 跳过已占用区域
      MemAllocation(m, base)
    }
    RegFileMap(block, regs, mems)
  }

  def summarize(map: RegFileMap): String = {
    val b = map.block
    val sb = new StringBuilder
    sb ++= s"Register Block : ${b.name}\n"
    sb ++= s"Device         : ${b.devName}\n"
    sb ++= s"Reg  Base      : 0x${b.regBaseAddress.toString(16)}\n"
    sb ++= s"Mem  Base      : 0x${b.memBaseAddress.toString(16)}\n"
    sb ++= s"Reg  Space     : ${map.totalRegByteSize} bytes\n"
    sb ++= s"Mem  Space     : ${map.totalMemByteSize} bytes\n\n"
    sb ++= "=== Registers ===\n"
    map.regs.foreach { a =>
      sb ++= s"  ${a.reg.name} @ 0x${a.byteOffset.toString(16)} (${a.byteSize}B, ${a.wordCount} word)\n"
      a.fieldAllocations.sortBy(_.bitOffset).foreach { fa =>
        val f = fa.field
        val bits = if (f.bitWidth == 1) s"[${fa.bitOffset}]" else s"[${fa.bitOffset + f.bitWidth - 1}:${fa.bitOffset}]"
        sb ++= s"    $bits ${f.name} (${f.access.id}, reset=${f.resetValue}) ${f.description}\n"
      }
    }
    if (map.mems.nonEmpty) {
      sb ++= "\n=== Memories ===\n"
      map.mems.foreach { m =>
        sb ++= s"  ${m.mem.name} @ 0x${m.baseAddress.toString(16)} (${m.mem.depth}x${m.mem.dataWidth}, ${m.mem.byteSize}B)\n"
      }
    }
    sb.toString
  }
}
