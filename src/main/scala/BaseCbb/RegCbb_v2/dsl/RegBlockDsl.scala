package BaseCbb.RegCbb_v2.dsl

import BaseCbb.RegCbb_v2._
import scala.collection.mutable.ArrayBuffer

/**
 * 字段级 DSL —— 推荐的寄存器定义入口。
 *
 * 用法：
 * {{{
 *   RegField("enable", 1) { f => f.rw().reset(0).desc("使能") }
 *   RegField.rw("baud_div", 12, 0, "波特率分频")
 *   RegField.ro("version", 4, 0, "版本号")
 *   RegField.w1c("tx_done", 1, "发送完成中断")
 * }}}
 */
class FieldBuilder {
  private var _name   = ""
  private var _width  = 1
  private var _access: AccessType = AccessType.RW
  private var _reset: BigInt = 0
  private var _desc   = ""
  private var _writeAct: WriteAction = WriteAction.Normal
  private var _enums: Map[BigInt, (String, String)] = Map.empty

  def named(n: String): this.type = { _name = n; this }
  def width(w: Int): this.type = { _width = w; this }
  def bits(w: Int): this.type = width(w)

  def rw(): this.type = { _access = AccessType.RW; this }
  def ro(): this.type = { _access = AccessType.RO; this }
  def wo(): this.type = { _access = AccessType.WO; this }
  def rc(): this.type = { _access = AccessType.RC; this }
  def rs(): this.type = { _access = AccessType.RS; this }
  def w1c(): this.type = { _access = AccessType.W1C; this }
  def w1s(): this.type = { _access = AccessType.W1S; this }
  def w1t(): this.type = { _access = AccessType.W1T; this }

  def reset(r: BigInt): this.type = { _reset = r; this }
  def desc(d: String): this.type = { _desc = d; this }

  def oneToClear(): this.type = { _writeAct = WriteAction.OneToClear; this }
  def oneToSet(): this.type = { _writeAct = WriteAction.OneToSet; this }
  def oneToToggle(): this.type = { _writeAct = WriteAction.OneToToggle; this }
  def clearOnRead(): this.type = { _writeAct = WriteAction.ClearOnRead; this }

  def enum(value: BigInt, name: String, desc: String = ""): this.type = {
    _enums += (value -> (name, desc)); this
  }

  def build(): RegFieldDef = {
    require(_name.nonEmpty, "field must have a name")
    RegFieldDef(_name, _width, _access, _reset, _desc, _writeAct, _enums)
  }
}

object RegField {
  /** 全功能入口：RegField("name", width) { f => f.rw().reset(0).desc("...") } */
  def apply(name: String, width: Int = 1)(block: FieldBuilder => Unit): RegFieldDef = {
    val b = new FieldBuilder
    b.named(name).width(width)
    block(b)
    b.build()
  }

  /** 便捷入口（单参数列表，Scala 2.13 兼容） */
  def rw(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "",
         block: FieldBuilder => Unit = _ => ()): RegFieldDef =
    RegField(name, width) { b => b.rw().reset(reset).desc(desc); block(b) }
  def rw(name: String, width: Int, desc: String): RegFieldDef = rw(name, width, 0, desc)

  def ro(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "",
         block: FieldBuilder => Unit = _ => ()): RegFieldDef =
    RegField(name, width) { b => b.ro().reset(reset).desc(desc); block(b) }
  def ro(name: String, width: Int, desc: String): RegFieldDef = ro(name, width, 0, desc)

  def wo(name: String, width: Int = 1, desc: String = "",
         block: FieldBuilder => Unit = _ => ()): RegFieldDef =
    RegField(name, width) { b => b.wo().desc(desc); block(b) }

  def rc(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "",
         block: FieldBuilder => Unit = _ => ()): RegFieldDef =
    RegField(name, width) { b => b.rc().reset(reset).desc(desc); block(b) }
  def rc(name: String, width: Int, desc: String): RegFieldDef = rc(name, width, 0, desc)

  def rs(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "",
         block: FieldBuilder => Unit = _ => ()): RegFieldDef =
    RegField(name, width) { b => b.rs().reset(reset).desc(desc); block(b) }
  def rs(name: String, width: Int, desc: String): RegFieldDef = rs(name, width, 0, desc)

  def w1c(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "",
          block: FieldBuilder => Unit = _ => ()): RegFieldDef =
    RegField(name, width) { b => b.w1c().reset(reset).desc(desc); block(b) }
  def w1c(name: String, width: Int, desc: String): RegFieldDef = w1c(name, width, 0, desc)

  def w1s(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "",
          block: FieldBuilder => Unit = _ => ()): RegFieldDef =
    RegField(name, width) { b => b.w1s().reset(reset).desc(desc); block(b) }
  def w1s(name: String, width: Int, desc: String): RegFieldDef = w1s(name, width, 0, desc)

  def w1t(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "",
          block: FieldBuilder => Unit = _ => ()): RegFieldDef =
    RegField(name, width) { b => b.w1t().reset(reset).desc(desc); block(b) }
  def w1t(name: String, width: Int, desc: String): RegFieldDef = w1t(name, width, 0, desc)
}

/** 寄存器 builder */
class RegBuilder(regName: String) {
  private val fields = ArrayBuffer[RegFieldDef]()
  private var _desc = ""
  private var _group: Option[String] = None
  private var _atomic = true

  def field(f: RegFieldDef): this.type = { fields += f; this }
  def desc(d: String): this.type = { _desc = d; this }
  def group(g: String): this.type = { _group = Some(g); this }
  /** 多字寄存器原子模式（默认）：写低字暂存、写最高字一次提交 */
  def atomic(): this.type = { _atomic = true; this }
  def atomic(a: Boolean): this.type = { _atomic = a; this }
  /** 多字寄存器非原子模式：逐字直接写 */
  def nonAtomic(): this.type = { _atomic = false; this }

  def build(): RegDef = RegDef(regName, fields.toSeq, _desc, _group, _atomic)
}

/** 存储器 builder */
class MemBuilder(memName: String) {
  private var _depth = 64
  private var _dataWidth = 32
  private var _memType: MemoryAccessType = MemoryAccessType.SP
  private var _base: Option[BigInt] = None   // None = 自动分配
  private var _desc = ""
  private var _atomic = true

  def depth(d: Int): this.type = { _depth = d; this }
  def dataWidth(w: Int): this.type = { _dataWidth = w; this }
  def width(w: Int): this.type = dataWidth(w)
  def sp(): this.type = { _memType = MemoryAccessType.SP; this }
  def tp(): this.type = { _memType = MemoryAccessType.TP; this }
  /** 手工指定基地址（可选；缺省由 AddressAllocator 从块 memBaseAddress 起自动分配） */
  def baseAddress(a: BigInt): this.type = { _base = Some(a); this }
  def desc(d: String): this.type = { _desc = d; this }
  /** 总线原子访问（memory 位宽 > 总线位宽时）：写低字暂存、写最高字一次提交 */
  def atomic(): this.type = { _atomic = true; this }
  def atomic(a: Boolean): this.type = { _atomic = a; this }
  def nonAtomic(): this.type = { _atomic = false; this }

  def build(): MemoryDef = MemoryDef(memName, _depth, _dataWidth, _memType, _base, _desc, _atomic)
}

/** 寄存器块 builder */
class BlockBuilder(blockName: String) {
  private val regs = ArrayBuffer[RegDef]()
  private val mems = ArrayBuffer[MemoryDef]()
  private var _desc = ""
  private var _device = blockName
  private var _regBase: BigInt = 0
  private var _memBase: BigInt = 0

  def device(d: String): this.type = { _device = d; this }
  def baseAddress(a: BigInt): this.type = { _regBase = a; this }
  def memBaseAddress(a: BigInt): this.type = { _memBase = a; this }
  def desc(d: String): this.type = { _desc = d; this }

  def reg(name: String)(block: RegBuilder => Unit): this.type = {
    val rb = new RegBuilder(name)
    block(rb)
    regs += rb.build()
    this
  }

  /** 直接追加已构造的寄存器（如从 RegBundle 转换而来） */
  def regs(rs: Seq[RegDef]): this.type = { regs ++= rs; this }

  def mem(name: String)(block: MemBuilder => Unit): this.type = {
    val mb = new MemBuilder(name)
    block(mb)
    mems += mb.build()
    this
  }

  def build(): RegBlockDef = RegBlockDef(blockName, _regBase, _memBase, regs.toSeq, mems.toSeq, _desc, _device)
}

object RegBlock {
  def apply(name: String)(block: BlockBuilder => Unit): RegBlockDef = {
    val b = new BlockBuilder(name)
    block(b)
    b.build()
  }
}
