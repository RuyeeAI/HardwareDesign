package BaseCbb.RegCbb_v2.gen

import BaseCbb.RegCbb_v2._

/**
 * 具名视图源码生成器：从 IR 生成一个 Scala 类，把"字符串键访问"
 * `view("ctrl")("baud_div")` 升级为编译期具名访问 `regs.ctrl.baud_div`。
 *
 * 把生成的文件加入工程后即可使用（见 demo 输出 UartRegs.scala）。
 */
object ViewSourceGen {

  def generate(map: RegFileMap, className: String, packageName: String): String = {
    val sb = new StringBuilder
    sb ++= s"package $packageName\n\n"
    sb ++= "import BaseCbb.RegCbb_v2.hw._\n\n"
    sb ++= s"/** 由 RegCbb_v2 自动生成：编译期具名寄存器视图 */\n"
    sb ++= s"class $className(view: RegView) {\n"
    map.regs.foreach { a =>
      sb ++= s"  /** ${a.reg.name} @ ${hex(a.byteOffset)}（${a.byteSize} B） */\n"
      sb ++= s"  val ${a.reg.name} = view(\"${a.reg.name}\")\n\n"
    }
    sb ++= "}\n"
    sb.toString
  }

  private def hex(v: BigInt): String = s"0x${v.toString(16)}"
}
