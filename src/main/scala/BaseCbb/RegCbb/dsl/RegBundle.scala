package BaseCbb.RegCbb.dsl

import chisel3._
import BaseCbb.RegCbb._

/**
 * GenBundle 风格（Bundle 式）寄存器域段定义 —— v2 自包含实现（不依赖 v1 已被删除的 GenBundle 基类）。
 *
 * 用法：
 * {{{
 *   class UartBundleRegs extends RegBundle {
 *     // 嵌套 RegBundle = 一个寄存器，内部字段紧凑排列（LSB-first）
 *     val bundle_ctrl = new RegBundle {
 *       val mode  = UInt(2.W)
 *       val burst = Bool()
 *       Attr += (mode  -> FieldAttr("工作模式", reset = 1))
 *       Attr += (burst -> FieldAttr("突发使能"))
 *     }
 *     // 命名后缀推断访问类型（_ro/_wo/_rc/_rs/_w1c/_w1s/_w1t），否则 RW
 *     val bundle_status_ro = new RegBundle {
 *       val link_up = Bool()
 *       Attr += (link_up -> FieldAttr("链路状态"))
 *     }
 *     // 叶子元素 = 单字段寄存器
 *     val bundle_scratch = UInt(8.W)
 *   }
 *
 *   // 转换并合入寄存器块
 *   RegBlock("uart") { b =>
 *     ...
 *     b.regs(BundleToRegDefs.toRegDefs(new UartBundleRegs))
 *   }
 * }}}
 */

/** 字段属性注解：描述 / 复位值 / 显式访问类型（缺省按命名后缀推断） */
case class FieldAttr(desc: String = "", reset: BigInt = 0, access: Option[AccessType] = None)

/** Bundle 式寄存器定义基类：每个顶层元素 = 一个寄存器 */
class RegBundle extends Bundle {
  var Attr: Map[Data, FieldAttr] = Map.empty
}

/** 把 RegBundle 转换为统一的 RegDef 序列（与字段级 DSL 汇合到同一 IR） */
object BundleToRegDefs {

  private val suffixMap: List[(String, AccessType)] = List(
    "_w1c" -> AccessType.W1C, "_w1s" -> AccessType.W1S, "_w1t" -> AccessType.W1T,
    "_ro"  -> AccessType.RO,  "_wo"  -> AccessType.WO,
    "_rc"  -> AccessType.RC,  "_rs"  -> AccessType.RS)

  /** 命名后缀推断访问类型（显式 FieldAttr.access 优先） */
  def inferAccess(name: String): AccessType =
    suffixMap.collectFirst { case (s, t) if name.endsWith(s) => t }.getOrElse(AccessType.RW)

  /** 提取一个寄存器（嵌套 RegBundle 或叶子元素） */
  private def toRegDef(regName: String, data: Data, topAttr: Map[Data, FieldAttr],
                       atomic: Boolean): RegDef = {
    // 寄存器名后缀推断的访问类型（如 status_ro → 该寄存器所有字段默认 RO）
    val regSuffixAccess: Option[AccessType] =
      suffixMap.collectFirst { case (s, t) if regName.endsWith(s) => t }
    def fieldAccess(fname: String, attr: FieldAttr): AccessType =
      attr.access.getOrElse(regSuffixAccess.getOrElse(inferAccess(fname)))

    val fields: Seq[RegFieldDef] = data match {
      case sub: RegBundle =>
        // 嵌套：字段紧凑排列（LSB-first）。
        // 注意：Chisel 反射返回的 elements 是声明顺序的逆序，需反转以保持定义顺序。
        sub.elements.toSeq.reverse.map { case (fname, fdata) =>
          val attr = sub.Attr.getOrElse(fdata, FieldAttr())
          RegFieldDef(fname, fdata.getWidth, fieldAccess(fname, attr), attr.reset, attr.desc)
        }
      case _ =>
        // 叶子：单字段寄存器（字段名 = 元素名）
        val attr = topAttr.getOrElse(data, FieldAttr())
        Seq(RegFieldDef(regName, data.getWidth, fieldAccess(regName, attr), attr.reset, attr.desc))
    }
    RegDef(regName, fields, description = "", group = None, atomic = atomic)
  }

  /**
   * 转换整个 bundle 为寄存器定义序列。
   * @param bundle RegBundle 实例（须在模块构造中实例化）
   * @param atomic 多字寄存器是否原子（默认原子）
   */
  def toRegDefs(bundle: RegBundle, atomic: Boolean = true): Seq[RegDef] =
    bundle.elements.toSeq.reverse.map { case (regName, data) =>
      toRegDef(regName, data, bundle.Attr, atomic)
    }

  /**
   * 把 RegBundle 转换为 memory entry 的扁平字段序列（LSB-first 紧凑排列）。
   *
   * 每个顶层元素 = 一个字段组：
   *  - 嵌套 RegBundle：内部字段紧凑排列（LSB-first），作为 entry 的连续域段；
   *  - 叶子元素：单字段（字段名 = 元素名）。
   * 字段位宽和 = entry 位宽（即 MemoryDef.dataWidth，由 MemoryDef.fromBundle 自动推导）。
   *
   * 用法：
   * {{{
   *   class FifoEntry extends RegBundle {
   *     val tag  = UInt(8.W)
   *     val data = UInt(24.W)
   *     Attr += (tag  -> FieldAttr("标签"))
   *     Attr += (data -> FieldAttr("数据"))
   *   }
   *   val mem = MemoryDef.fromBundle("fifo", 64, BundleToRegDefs.toEntryFields(new FifoEntry))
   *   // dataWidth = 32（8+24），tag[7:0]、data[31:8]
   * }}}
   */
  def toEntryFields(bundle: RegBundle): Seq[RegFieldDef] = {
    def fieldOf(fname: String, fdata: Data, attr: FieldAttr, regSuffix: Option[AccessType]): RegFieldDef = {
      val access = attr.access.getOrElse(regSuffix.getOrElse(inferAccess(fname)))
      RegFieldDef(fname, fdata.getWidth, access, attr.reset, attr.desc)
    }
    bundle.elements.toSeq.reverse.flatMap { case (regName, data) =>
      val regSuffix = suffixMap.collectFirst { case (s, t) if regName.endsWith(s) => t }
      data match {
        case sub: RegBundle =>
          // 嵌套：内部字段紧凑排列（LSB-first），作为 entry 连续域段
          sub.elements.toSeq.reverse.map { case (fname, fdata) =>
            val attr = sub.Attr.getOrElse(fdata, FieldAttr())
            fieldOf(fname, fdata, attr, regSuffix)
          }
        case _ =>
          // 叶子：单字段（字段名 = 元素名）
          val attr = bundle.Attr.getOrElse(data, FieldAttr())
          Seq(fieldOf(regName, data, attr, regSuffix))
      }
    }
  }

  /** 便捷入口：由 RegBundle 直接构造纯寄存器块（RegBlockDef；不含存储器） */
  def toBlock(name: String, bundle: RegBundle,
              description: String = ""): RegBlockDef =
    RegBlockDef(name, toRegDefs(bundle), description)

  /** 便捷入口：由 RegBundle 直接构造功能模块（寄存器块自动并入） */
  def toModule(name: String, bundle: RegBundle,
               regBlockName: String = "regs",
               baseAddress: Option[BigInt] = None,
               memBaseAddress: Option[BigInt] = None,
               description: String = ""): ModuleDef =
    ModuleDef(name, Seq(RegBlockDef(regBlockName, toRegDefs(bundle), description)),
      Seq.empty, baseAddress, memBaseAddress, description)
}
