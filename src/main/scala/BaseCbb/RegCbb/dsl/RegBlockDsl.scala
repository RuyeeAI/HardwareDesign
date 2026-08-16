package BaseCbb.RegCbb.dsl

import BaseCbb.RegCbb._
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

/** 寄存器 builder（寄存器块内） */
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

/** 存储器 builder（存储器块内） */
class MemBuilder(memName: String) {
  private var _depth = 64
  private var _dataWidth = 32
  private var _memType: MemoryAccessType = MemoryAccessType.SP
  private var _base: Option[BigInt] = None   // None = 自动分配
  private var _desc = ""
  private var _atomic = true
  private var _entryFields: Seq[RegFieldDef] = Seq.empty

  def depth(d: Int): this.type = { _depth = d; this }
  def dataWidth(w: Int): this.type = { _dataWidth = w; this }
  def width(w: Int): this.type = dataWidth(w)
  def sp(): this.type = { _memType = MemoryAccessType.SP; this }
  def tp(): this.type = { _memType = MemoryAccessType.TP; this }
  /** 手工指定基地址（可选；缺省由 AddressAllocator 自动分配） */
  def baseAddress(a: BigInt): this.type = { _base = Some(a); this }
  def desc(d: String): this.type = { _desc = d; this }
  /** 总线原子访问（memory 位宽 > 总线位宽时）：写低字暂存、写最高字一次提交 */
  def atomic(): this.type = { _atomic = true; this }
  def atomic(a: Boolean): this.type = { _atomic = a; this }
  def nonAtomic(): this.type = { _atomic = false; this }

  /** entry 域段信息：显式指定字段序列（位宽和必须 == dataWidth） */
  def entryFields(fs: Seq[RegFieldDef]): this.type = { _entryFields = fs; this }
  /** entry 域段信息：来自 RegBundle（dataWidth 自动取字段位宽和） */
  def bundle(b: RegBundle): this.type = {
    _entryFields = BundleToRegDefs.toEntryFields(b)
    _dataWidth = _entryFields.map(_.bitWidth).sum
    this
  }

  def build(): MemoryDef = {
    if (_entryFields.nonEmpty && _entryFields.map(_.bitWidth).sum != _dataWidth)
      sys.error(s"memory '$memName': entryFields total width ${_entryFields.map(_.bitWidth).sum} != dataWidth ${_dataWidth}")
    MemoryDef(memName, _depth, _dataWidth, _memType, _base, _desc, _atomic, _entryFields)
  }
}

// ==================== RegBlock（纯寄存器块） ====================

/** 寄存器块 builder：只含寄存器（功能片段） */
class RegBlockBuilder(blockName: String) {
  private val regs = ArrayBuffer[RegDef]()
  private var _desc = ""

  def desc(d: String): this.type = { _desc = d; this }

  def reg(name: String)(block: RegBuilder => Unit): this.type = {
    val rb = new RegBuilder(name)
    block(rb)
    regs += rb.build()
    this
  }

  /** 直接追加已构造的寄存器（如从 RegBundle 转换而来） */
  def regs(rs: Seq[RegDef]): this.type = { regs ++= rs; this }

  def build(): RegBlockDef = RegBlockDef(blockName, regs.toSeq, _desc)
}

object RegBlock {
  /** 纯寄存器块：RegBlock("ctrl_regs") { b => b.reg("ctrl"){...} } */
  def apply(name: String)(block: RegBlockBuilder => Unit): RegBlockDef = {
    val b = new RegBlockBuilder(name)
    block(b)
    b.build()
  }
}

// ==================== MemBlock（纯存储器块） ====================

/** 存储器块 builder：只含存储器 */
class MemBlockBuilder(blockName: String) {
  private val mems = ArrayBuffer[MemoryDef]()
  private var _desc = ""

  def desc(d: String): this.type = { _desc = d; this }

  def mem(name: String)(block: MemBuilder => Unit): this.type = {
    val mb = new MemBuilder(name)
    block(mb)
    mems += mb.build()
    this
  }

  /** 直接追加已构造的存储器 */
  def mems(ms: Seq[MemoryDef]): this.type = { mems ++= ms; this }

  def build(): MemBlockDef = MemBlockDef(blockName, mems.toSeq, _desc)
}

object MemBlock {
  /** 纯存储器块：MemBlock("fifo_mems") { mb => mb.mem("fifo"){...} } */
  def apply(name: String)(block: MemBlockBuilder => Unit): MemBlockDef = {
    val b = new MemBlockBuilder(name)
    block(b)
    b.build()
  }
}

// ==================== Module（功能模块） ====================

/** 功能模块 builder：多个寄存器块 + 多个存储器块 */
class ModuleBuilder(modName: String) {
  private val regBlocks = ArrayBuffer[RegBlockDef]()
  private val memBlocks = ArrayBuffer[MemBlockDef]()
  private val directRegs = ArrayBuffer[RegDef]()   // 便捷 reg() 累积
  private val directMems = ArrayBuffer[MemoryDef]() // 便捷 mem() 累积
  private var _base: Option[BigInt] = None
  private var _memBase: Option[BigInt] = None
  private var _desc = ""

  /** 模块基址：None = 系统自动分配；Some = 手工指定 */
  def baseAddress(a: BigInt): this.type = { _base = Some(a); this }
  def baseAddress(a: Option[BigInt]): this.type = { _base = a; this }
  def autoAddress(): this.type = { _base = None; this }
  /** 模块存储器区基址：None = 自动紧随寄存器区之后；Some = 手工指定 */
  def memBaseAddress(a: BigInt): this.type = { _memBase = Some(a); this }
  def memBaseAddress(a: Option[BigInt]): this.type = { _memBase = a; this }
  def autoMemAddress(): this.type = { _memBase = None; this }
  def desc(d: String): this.type = { _desc = d; this }

  def regBlock(rb: RegBlockDef): this.type = { regBlocks += rb; this }
  def memBlock(mb: MemBlockDef): this.type = { memBlocks += mb; this }

  /** 便捷：模块内直接定义寄存器（build 时自动合并为 modName+"_regs" 单块） */
  def reg(name: String)(block: RegBuilder => Unit): this.type = {
    val rb = new RegBuilder(name)
    block(rb)
    directRegs += rb.build()
    this
  }
  /** 便捷：模块内直接定义存储器（build 时自动合并为 modName+"_mems" 单块） */
  def mem(name: String)(block: MemBuilder => Unit): this.type = {
    val mb = new MemBuilder(name)
    block(mb)
    directMems += mb.build()
    this
  }

  def build(): ModuleDef = {
    if (directRegs.nonEmpty) regBlocks += RegBlockDef(modName + "_regs", directRegs.toSeq)
    if (directMems.nonEmpty) memBlocks += MemBlockDef(modName + "_mems", directMems.toSeq)
    ModuleDef(modName, regBlocks.toSeq, memBlocks.toSeq, _base, _memBase, _desc)
  }
}

object FuncModule {
  /** 功能模块：FuncModule("uart") { m => m.regBlock(rb); m.memBlock(mb); m.baseAddress(0x40000000) } */
  def apply(name: String)(block: ModuleBuilder => Unit): ModuleDef = {
    val b = new ModuleBuilder(name)
    block(b)
    b.build()
  }
}

// ==================== System（系统） ====================

/** 系统 builder：多个功能模块（模块间地址自动/手工） */
class SystemBuilder(sysName: String) {
  private val mods = ArrayBuffer[ModuleDef]()
  private var _device = sysName
  private var _desc = ""

  def device(d: String): this.type = { _device = d; this }
  def desc(d: String): this.type = { _desc = d; this }

  def module(m: ModuleDef): this.type = { mods += m; this }

  def build(): SystemDef = SystemDef(sysName, mods.toSeq, _device, _desc)
}

object System {
  /** 系统：System("soc") { s => s.module(m1); s.module(m2) } */
  def apply(name: String)(block: SystemBuilder => Unit): SystemDef = {
    val b = new SystemBuilder(name)
    block(b)
    b.build()
  }
}
