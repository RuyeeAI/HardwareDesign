package BaseCbb.data

import scala.collection.mutable.ArrayBuffer

/** 设计参数容器：子类把参数写成 `val`，可选地用 `Desc` 补描述，再通过 `toIR` 反射导出。
  *
  * 由 HBS `BaseCbb/utils/GeneratorLib.scala` 的 `GenParam` 迁入（语义保持一致），
  * 供 `HBS.top.HbsParams` / `HBS.tm.TmParam` / `HBS.swf.common.SwfParams` 继承。
  */
class GenParam {
  var Desc: Map[String, String] = Map()

  def toIR(): Seq[Map[String, Any]] = {
    val fields = this.getClass.getDeclaredFields
    val pMap = ArrayBuffer[Map[String, Any]]()
    fields.foreach { x =>
      // 过滤 scalac 为构造器插入的 bitmap$init$0 位图字段
      if (!x.getName.contains("bitmap$init$0")) {
        val desc = Desc.getOrElse(x.getName, "Warning: Please add description for " + x.getName)
        pMap += Map(
          "Name"  -> x.getName,
          "Desc"  -> desc,
          "Value" -> this.getClass.getMethod(x.getName).invoke(this)
        )
      }
    }
    pMap.toSeq
  }
}
