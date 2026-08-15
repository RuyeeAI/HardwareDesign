package BaseCbb.RegCbb.gen

import BaseCbb.RegCbb._

/** JSON IR 生成器（手写序列化，无外部依赖） */
object JsonGen {

  def generate(map: RegFileMap): String = {
    val b = map.block
    val sb = new StringBuilder
    sb ++= "{\n"
    sb ++= s"""  "deviceName": ${q(b.devName)},\n"""
    sb ++= s"""  "displayName": ${q(b.name)},\n"""
    sb ++= s"""  "description": ${q(b.description)},\n"""
    sb ++= s"""  "regBaseAddress": ${q(hex(b.regBaseAddress))},\n"""
    sb ++= s"""  "memBaseAddress": ${q(hex(b.memBaseAddress))},\n"""
    sb ++= s"""  "totalRegByteSize": ${map.totalRegByteSize},\n"""
    sb ++= s"""  "totalMemByteSize": ${map.totalMemByteSize},\n"""
    sb ++= "  \"registers\": [\n"
    map.regs.zipWithIndex.foreach { case (a, i) =>
      sb ++= "    {\n"
      sb ++= s"""      "name": ${q(a.reg.name)},\n"""
      sb ++= s"""      "byteOffset": ${q(hex(a.byteOffset))},\n"""
      sb ++= s"""      "byteSize": ${a.byteSize},\n"""
      sb ++= s"""      "wordCount": ${a.wordCount},\n"""
      sb ++= s"""      "atomic": ${a.reg.atomic},\n"""
      sb ++= s"""      "description": ${q(a.reg.description)},\n"""
      sb ++= s"""      "group": ${q(a.reg.group.getOrElse(""))},\n"""
      sb ++= "      \"fields\": [\n"
      a.fieldAllocations.zipWithIndex.foreach { case (fa, j) =>
        val f = fa.field
        sb ++= "        {\n"
        sb ++= s"""          "name": ${q(f.name)},\n"""
        sb ++= s"""          "bitOffset": ${fa.bitOffset},\n"""
        sb ++= s"""          "bitWidth": ${f.bitWidth},\n"""
        sb ++= s"""          "access": ${q(f.access.id)},\n"""
        sb ++= s"""          "writeAction": ${q(f.writeAction.id)},\n"""
        sb ++= s"""          "resetValue": ${f.resetValue},\n"""
        sb ++= s"""          "description": ${q(f.description)},\n"""
        sb ++= "          \"enumerations\": {"
        sb ++= f.enumerations.toSeq.sortBy(_._1).map { case (v, (n, d)) =>
          s"${q(v.toString)}: ${q(if (d.nonEmpty) s"$n: $d" else n)}"
        }.mkString(", ")
        sb ++= "}\n"
        sb ++= "        }" + (if (j < a.fieldAllocations.size - 1) "," else "") + "\n"
      }
      sb ++= "      ]\n"
      sb ++= "    }" + (if (i < map.regs.size - 1) "," else "") + "\n"
    }
    sb ++= "  ],\n"
    sb ++= "  \"memories\": [\n"
    map.mems.zipWithIndex.foreach { case (ma, i) =>
      val m = ma.mem
      sb ++= "    {\n"
      sb ++= s"""      "name": ${q(m.name)},\n"""
      sb ++= s"""      "baseAddress": ${q(hex(ma.baseAddress))},\n"""
      sb ++= s"""      "depth": ${m.depth},\n"""
      sb ++= s"""      "dataWidth": ${m.dataWidth},\n"""
      sb ++= s"""      "byteSize": ${m.byteSize},\n"""
      sb ++= s"""      "memType": ${q(m.memType.id)},\n"""
      sb ++= s"""      "atomic": ${m.atomic},\n"""
      sb ++= s"""      "description": ${q(m.description)}\n"""
      sb ++= "    }" + (if (i < map.mems.size - 1) "," else "") + "\n"
    }
    sb ++= "  ]\n"
    sb ++= "}\n"
    sb.toString
  }

  private def q(s: String): String = "\"" + esc(s) + "\""
  private def esc(s: String): String = s.flatMap {
    case '"'  => "\\\""
    case '\\' => "\\\\"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case c if c < 0x20 => f"\\u${c.toInt}%04x"
    case c => c.toString
  }
  private def hex(v: BigInt): String = s"0x${v.toString(16)}"
}
