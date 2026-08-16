package BaseCbb.RegCbb.gen

import BaseCbb.RegCbb._

/** Markdown 寄存器手册生成器（含地址映射、位域图、字段表、枚举说明） */
object MarkdownGen {

  def generate(map: RegFileMap): String = {
    val sb = new StringBuilder
    sb ++= s"# ${map.deviceName} 寄存器手册\n\n"
    sb ++= s"> 由 RegCbb 自动生成 · 寄存器基地址 `${hex(map.regBaseAddress)}` · 存储器基地址 `${hex(map.memBaseAddress)}`\n\n"
    if (map.description.nonEmpty) sb ++= s"${map.description}\n\n"

    // 地址映射总表
    sb ++= "## 地址映射\n\n"
    sb ++= "| 偏移 | 名称 | 大小 | 访问/原子 | 复位值 | 说明 |\n|---|---|---|---|---|---|\n"
    map.regs.foreach { a =>
      val atom = if (a.wordCount > 1) (if (a.reg.atomic) "多字·原子" else "多字·非原子") else "-"
      sb ++= s"| ${hex(a.byteOffset)} | `${a.reg.name}` | ${a.byteSize} B | $atom | ${hex(resetOf(a))} | ${a.reg.description} |\n"
    }
    if (map.mems.nonEmpty) {
      map.mems.foreach { ma =>
        val atom = if (ma.mem.wordCount > 1) (if (ma.mem.atomic) "原子" else "非原子") else "-"
        sb ++= s"| ${hex(ma.baseAddress)} | `${ma.mem.name}`（存储器） | ${ma.mem.byteSize} B | $atom | - | ${ma.mem.description} |\n"
      }
    }
    sb ++= "\n"

    // 每寄存器详表
    map.regs.foreach { a =>
      val group = a.reg.group.map(g => s"（分组：$g）").getOrElse("")
      val atom = if (a.wordCount > 1) (if (a.reg.atomic) " · **多字原子**（写低字暂存，写最高字提交）" else " · 多字非原子（逐字直接写）") else ""
      sb ++= s"## ${a.reg.name} $group$atom\n\n"
      sb ++= s"偏移 `${hex(a.byteOffset)}` · ${a.byteSize} B · 复位 `${hex(resetOf(a))}`\n\n"
      if (a.reg.description.nonEmpty) sb ++= s"${a.reg.description}\n\n"
      sb ++= bitDiagram(a)
      sb ++= "\n\n| 位 | 字段 | 访问 | 复位 | 描述 |\n|---|---|---|---|---|\n"
      a.fieldAllocations.sortBy(-_.bitOffset).foreach { fa =>
        val f = fa.field
        val bits = if (f.bitWidth == 1) s"[${fa.bitOffset}]" else s"[${fa.bitOffset + f.bitWidth - 1}:${fa.bitOffset}]"
        val enums = f.enumerations.toSeq.sortBy(_._1).map { case (v, (n, d)) =>
          s"<br/>`0x${v.toString(16)}` = $n${if (d.nonEmpty) s"（$d）" else ""}"
        }.mkString
        sb ++= s"| $bits | `${f.name}` | ${f.access.id} | ${hex(f.resetValue)} | ${f.description}$enums |\n"
      }
      sb ++= "\n"
    }

    if (map.mems.nonEmpty) {
      sb ++= "## 存储器\n\n"
      sb ++= "| 名称 | 基地址 | 深度 | 位宽 | 大小 | 类型 | 说明 |\n|---|---|---|---|---|---|---|\n"
      map.mems.foreach { ma =>
        sb ++= s"| `${ma.mem.name}` | ${hex(ma.baseAddress)} | ${ma.mem.depth} | ${ma.mem.dataWidth} | ${ma.mem.byteSize} B | ${ma.mem.memType.id} | ${ma.mem.description} |\n"
        if (ma.mem.entryFields.nonEmpty) {
          sb ++= s"\n**${ma.mem.name} entry 域段**（位宽 ${ma.mem.dataWidth}，LSB-first）：\n\n"
          sb ++= "| 位 | 字段 | 访问 | 复位 | 描述 |\n|---|---|---|---|---|\n"
          ma.mem.entryFields.zip(ma.mem.entryFieldOffsets).sortBy(-_._2).foreach { case (f, bitOffset) =>
            val bits = if (f.bitWidth == 1) s"[${bitOffset}]" else s"[${bitOffset + f.bitWidth - 1}:${bitOffset}]"
            val enums = f.enumerations.toSeq.sortBy(_._1).map { case (v, (n, d)) =>
              s"<br/>`0x${v.toString(16)}` = $n${if (d.nonEmpty) s"（$d）" else ""}"
            }.mkString
            sb ++= s"| $bits | `${f.name}` | ${f.access.id} | ${hex(f.resetValue)} | ${f.description}$enums |\n"
          }
          sb ++= "\n"
        }
      }
      sb ++= "\n"
    }

    sb ++= "---\n*本文档由 RegCbb 自动生成，请勿手工修改。*\n"
    sb.toString
  }

  /** 复位值 = 各字段复位值按位偏移折叠 */
  private def resetOf(a: RegAllocation): BigInt =
    a.fieldAllocations.foldLeft(BigInt(0)) { case (acc, fa) => acc | (fa.field.resetValue << fa.bitOffset) }

  /** 文本位域图（MSB → LSB） */
  private def bitDiagram(a: RegAllocation): String = {
    val total = a.totalBits
    val fas = a.fieldAllocations.sortBy(-_.bitOffset) // MSB first
    val cells = fas.map { fa =>
      val w = math.max(fa.field.bitWidth * 2, fa.field.name.length + 2)
      (fa, w)
    }
    val totalW = cells.map(_._2).sum
    if (totalW > 120 || total > 96) {
      // 过宽时退化为行列表
      return fas.map(fa =>
        s"`[${fa.bitOffset + fa.field.bitWidth - 1}:${fa.bitOffset}]` ${fa.field.name}（${fa.field.access.id}）").mkString("  \n")
    }
    def border: String = "+" + cells.map(c => "-" * c._2).mkString("+") + "+"
    def place(buf: Array[Char], pos: Int, s: String, rightAlign: Boolean): Unit = {
      val start = if (rightAlign) math.max(0, pos - s.length + 1) else pos
      for (i <- 0 until s.length) {
        val p = start + i
        if (p >= 0 && p < buf.length) buf(p) = s.charAt(i)
      }
    }
    // 位刻度行（格子足够宽才同时标注 hi/lo，避免重叠）
    val ruler = Array.fill(totalW)(' ')
    var pos = 0
    place(ruler, 0, (total - 1).toString, rightAlign = false)
    cells.foreach { case (fa, w) =>
      val hiS = fa.hi.toString
      val loS = fa.bitOffset.toString
      if (w >= hiS.length + loS.length + 2) {
        place(ruler, pos, hiS, rightAlign = false)
        place(ruler, pos + w - 1, loS, rightAlign = true)
      } else if (w >= hiS.length + 1) {
        place(ruler, pos, hiS, rightAlign = false)
      }
      pos += w
    }
    // 字段名行
    val names = Array.fill(totalW)(' ')
    pos = 0
    cells.foreach { case (fa, w) =>
      val n = fa.field.name
      val start = pos + math.max(0, (w - n.length) / 2)
      place(names, start, n.take(w), rightAlign = false)
      pos += w
    }
    s"""```
${ruler.mkString}
${border}
${names.mkString}
${border}
```"""
  }

  private def hex(v: BigInt): String = s"0x${v.toString(16)}"
}
