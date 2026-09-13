package BaseCbb.data

import chisel3._
import BaseCbb.memory.Memory
import scala.collection.mutable.ArrayBuffer

/** 设计描述 → IR 的导出机件（「参数 / 存储清单 / 数据结构」三类信息汇总成 JSON 可序列化的 Map）。
  *
  * 由 hardware-design / HBS 的 `BaseCbb/utils/GeneratorLib.scala` 迁入，但**不再保留其中重复的
  * `Memory` case class 与 `MemoryAccessType` 枚举** —— 那两个符号统一复用
  * `BaseCbb.memory.Memory` / `BaseCbb.memory.MemoryAccessType`（HD 版本字段更全、`toMap` 的键
  * 与 IR 约定完全一致：Name/AccessType/Width/Depth/InstNum）。
  *
  * 与 GenParam（`GenParam.toIR`）配套，使用入口见 `Demo.DemoGenIR`。
  */
object GenFieldListFromBundle {

  private def FieldMap(name: String, width: Int, Desc: String): Map[String, Any] = Map(
    "Name" -> name,
    "Width" -> width,
    "Desc" -> Desc
  )

  /** 递归展开一个 Bundle（或 Vec）的字段清单：名字 / 位宽 / 描述（来自 `GenBundle.Attr`）。 */
  def apply(tt: Data, name_d: String = "", desc_d: String = ""): ArrayBuffer[Map[String, Any]] = {
    val FieldList = ArrayBuffer[Map[String, Any]]()
    tt match {
      case t: GenBundle =>
        val Attr = t.Attr
        for (fld <- t.elements) {
          var desc = "Warning: Please add description to the field!!"
          var expand = true
          if (Attr.contains(fld._2)) {
            desc = Attr(fld._2).Desc
            expand = Attr(fld._2).ExpandArr
          }
          fld._2 match {
            case datas: Vec[Data] @unchecked =>
              if (expand) {
                val fl = GenFieldListFromBundle(datas, fld._1, desc)
                fl.foreach(x => FieldList += FieldMap(fld._1 + "_" + x("Name"), x("Width").toString.toInt, x("Desc").toString))
              } else {
                FieldList += FieldMap(fld._1, fld._2.getWidth, desc)
              }
            case datas: GenBundle =>
              GenFieldListFromBundle(datas, fld._1, desc).foreach(FieldList += _)
            case _ =>
              FieldList += FieldMap(fld._1, fld._2.getWidth, desc)
          }
        }
        FieldList.reverse
      case datas: Vec[Data] @unchecked =>
        val vec_field_list = ArrayBuffer[Map[String, Any]]()
        val ele = datas.head
        val len = datas.length
        ele match {
          case _: Bool =>
            (0 until len).foreach(i => vec_field_list += FieldMap(i.toString, ele.getWidth, desc_d))
          case _: UInt =>
            (0 until len).foreach(i => vec_field_list += FieldMap(i.toString, ele.getWidth, desc_d))
          case bundle: GenBundle =>
            val sf = GenFieldListFromBundle(bundle, name_d, desc_d)
            for (i <- 0 until len; f <- sf) {
              vec_field_list += FieldMap(i + "_" + f("Name").toString, f("Width").toString.toInt, f("Desc").toString)
            }
          case vd: Vec[Data] @unchecked =>
            val sf = GenFieldListFromBundle(vd, name_d, desc_d)
            for (i <- 0 until len; f <- sf) {
              vec_field_list += FieldMap(i.toString + "_" + f("Name").toString, f("Width").toString.toInt, f("Desc").toString)
            }
          case _ =>
            println("Error: Not support data type!")
        }
        vec_field_list
      case _ =>
        println("Error: Not support data type!")
        FieldList
    }
  }
}

/** 数据结构清单：名字 → Bundle 定义。 */
abstract class GenDataStructure {
  var DataStructMap: Map[String, GenBundle] = Map()

  def toIR: Map[String, Any] = {
    var targetMap: Map[String, Any] = Map()
    for (i <- DataStructMap.keys) {
      targetMap += (i -> GenFieldListFromBundle(DataStructMap(i)))
    }
    targetMap
  }
}

/** 存储清单：IR `Memory` 描述符数组（即 `BaseCbb.memory.Memory`）。 */
abstract class GenMemory {
  var MemoryArr: ArrayBuffer[Memory] = ArrayBuffer()

  def toIR(): Seq[Map[String, Any]] = MemoryArr.map(_.toMap).toSeq
}

/** IR 汇总：参数 + 存储清单 + 数据结构。 */
abstract class GenIR {
  var IR: Map[String, Any] = Map()
  val MemoryList: GenMemory
  val Param: GenParam
  val DataStruct: GenDataStructure

  def toIR: Map[String, Any] = {
    IR += ("Parameter" -> Param.toIR)
    IR += ("MemoryList" -> MemoryList.toIR)
    IR += ("DataStructure" -> DataStruct.toIR)
    IR
  }
}
