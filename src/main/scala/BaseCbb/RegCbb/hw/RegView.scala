package BaseCbb.RegCbb.hw

import chisel3._
import BaseCbb.RegCbb._

/**
 * 用户逻辑连接视图（命名访问 + 自动位域切割）。
 *
 * 用法：
 * {{{
 *   val map = AddressAllocator.allocate(block)
 *   val regFile = Module(new RegFileTop(map))
 *   val regs = RegView(map, regFile.io.user)
 *
 *   // 写侧（捕获式）：写脉冲 + 数据（同拍）
 *   when(regs("tx_data_wo").wrEn) { txByte := regs("tx_data_wo").field("data").wrData }
 *   // 读当前值
 *   val baud = regs("ctrl").field("baud_div").value
 *   // RO 字段驱动（字段级，真实端口可直接赋值）
 *   regs("ctrl").field("version").roValue := version
 *   // W1C 字段：硬件置位
 *   regs("irq_w1c").field("tx_done").hwSet := txDonePulse
 * }}}
 */
class FieldHandle(reg: RegHandle, fa: FieldAllocation) {
  val name: String = fa.field.name
  val bitOffset: Int = fa.bitOffset
  val width: Int = fa.field.bitWidth
  val access: AccessType = fa.field.access
  val resetValue: BigInt = fa.field.resetValue
  val description: String = fa.field.description
  private def hi: Int = bitOffset + width - 1

  /** 寄存器级写脉冲（SW 写该寄存器任一可写字段时拉高一拍） */
  def wrEn: Bool = reg.wrEn
  /** 寄存器级读脉冲 */
  def rdEn: Bool = reg.rdEn
  /** SW 写入数据（按本字段位域切割，与 wrEn 同拍有效） */
  def wrData: UInt = reg.wrData(hi, bitOffset)
  /** 读回数据（按本字段位域切割，与 rdEn 同拍） */
  def rdData: UInt = reg.rdData(hi, bitOffset)
  /** 当前值（SW 视角，含 RO 位） */
  def value: UInt = reg.value(hi, bitOffset)

  /** RO 字段驱动端口（真实 Input，可直接 `:=` 赋值） */
  def roValue: UInt = fieldInput(reg.core.ro.value, name)
  /** W1C/RC 字段置位端口（电平，或 1） */
  def hwSet: UInt = fieldInput(reg.core.hwSet.bits, name)
  /** W1S/RS 字段清除端口（电平，与 0） */
  def hwClr: UInt = fieldInput(reg.core.hwClr.bits, name)
  /** W1T 字段翻转端口（电平，异或） */
  def hwTog: UInt = fieldInput(reg.core.hwTog.bits, name)
  /** RW 字段硬件直写数据端口（配合 reg.hwWrEn） */
  def hwWrData: UInt = fieldInput(reg.core.hwWr.data, name)

  private def fieldInput(rec: Record, fname: String): UInt =
    rec.elements
      .getOrElse(fname,
        sys.error(s"field '$fname' of register '${reg.name}' has no such hardware input " +
          "(check access type / use the right accessor)"))
      .asInstanceOf[UInt]
}

class RegHandle(val name: String, val core: RegUserIO, val alloc: RegAllocation) {
  def totalBits: Int = alloc.totalBits
  def byteOffset: BigInt = alloc.byteOffset
  def fields: Seq[FieldHandle] = alloc.fieldAllocations.map(fa => new FieldHandle(this, fa))
  def field(fname: String): FieldHandle = {
    val fa = alloc.fieldAllocations.find(_.field.name == fname).getOrElse(
      sys.error(s"field '$fname' not found in register '$name', available: " +
        alloc.fieldAllocations.map(_.field.name).mkString(", ")))
    new FieldHandle(this, fa)
  }
  def apply(fname: String): FieldHandle = field(fname)

  def wrEn: Bool = core.sw.wrEn
  def wrData: UInt = core.sw.wrData
  def rdEn: Bool = core.sw.rdEn
  def rdData: UInt = core.sw.rdData
  def value: UInt = core.sw.value
  /** RW 硬件直写使能（寄存器级） */
  def hwWrEn: Bool = core.hwWr.en

  // ---- 统一 Bundle 接口（RegUserIO）的类型化子接口（推荐写法） ----
  /** 寄存器对用户侧的统一接口 Bundle（按访问类型分组） */
  def user: RegUserIO = core
  /** SW→HW 事件视图：regs("x").sw.wrEn / .sw.wrData / .sw.value */
  def sw: RegSwIO = core.sw
  /** RO 字段驱动：regs("x").ro.value("fname") := ... */
  def ro: RoHwIF = core.ro
  /** W1C/RC 置位：regs("x").hwSet.bits("fname") := ... */
  def hwSet: HwSetIF = core.hwSet
  /** W1S/RS 清除：regs("x").hwClr.bits("fname") := ... */
  def hwClr: HwClrIF = core.hwClr
  /** W1T 翻转：regs("x").hwTog.bits("fname") := ... */
  def hwTog: HwTogIF = core.hwTog
  /** RW 硬件直写：regs("x").hwWr.en := ...；regs("x").hwWr.data("fname") := ... */
  def hwWr: HwWrIF = core.hwWr
}

class RegView(map: RegFileMap, user: RegUserRecord) {
  // 用户侧输入默认置 0 / false（父模块作用域内的连接，满足 Chisel "sink not fully
  // initialized" 检查；用户随后对字段的显式连接会覆盖这些默认值）。
  // 注意：不能用 DontCare —— hwWr.en/hwSet.bits 等会被硬件逻辑消费，DontCare 在仿真中
  // 表现为 x，会随机改写寄存器内容。
  map.regs.foreach { a =>
    val core = user.elements(a.reg.name).asInstanceOf[RegUserIO]
    core.hwWr.en := false.B
    core.ro.value.elements.foreach  { case (_, v) => v := 0.U(v.getWidth.W) }
    core.hwSet.bits.elements.foreach { case (_, v) => v := 0.U(v.getWidth.W) }
    core.hwClr.bits.elements.foreach { case (_, v) => v := 0.U(v.getWidth.W) }
    core.hwTog.bits.elements.foreach { case (_, v) => v := 0.U(v.getWidth.W) }
    core.hwWr.data.elements.foreach { case (_, v) => v := 0.U(v.getWidth.W) }
  }

  private val cores: Map[String, RegUserIO] =
    map.regs.map(a => a.reg.name -> user.elements(a.reg.name).asInstanceOf[RegUserIO]).toMap
  private val byName: Map[String, RegAllocation] =
    map.regs.map(a => a.reg.name -> a).toMap

  def reg(name: String): RegHandle = {
    val alloc = byName.getOrElse(name,
      sys.error(s"register '$name' not found, available: ${names.mkString(", ")}"))
    new RegHandle(name, cores(name), alloc)
  }
  def apply(name: String): RegHandle = reg(name)
  def names: Seq[String] = map.regs.map(_.reg.name)
  def all: Seq[RegHandle] = map.regs.map(a => reg(a.reg.name))
  def writableRegs: Seq[RegHandle] = all.filter(_.fields.exists(_.access.swWritable))
  def roRegs: Seq[RegHandle] = all.filter(_.fields.forall(_.access == AccessType.RO))
  def readNotifyRegs: Seq[RegHandle] =
    all.filter(_.fields.exists(f => f.access == AccessType.RC || f.access == AccessType.RS))
}

object RegView {
  def apply(map: RegFileMap, user: RegUserRecord): RegView = new RegView(map, user)

  /** 简单总线版：未挂用户侧逻辑时，存储器响应输入默认 0（挂接后会覆盖） */
  def apply(map: RegFileMap, top: RegFileTop): RegView = {
    val v = new RegView(map, top.io.user)
    top.io.memPorts.elements.foreach { case (_, p) =>
      val port = p.asInstanceOf[MemPortIO]
      port.rdata := 0.U(port.rdata.getWidth.W)
      port.ack := false.B
      port.status := 0.U(3.W)
    }
    v
  }

  /** AXI-Lite 版 */
  def apply(map: RegFileMap, top: AxiLiteRegFile): RegView = {
    val v = new RegView(map, top.io.user)
    top.io.memPorts.elements.foreach { case (_, p) =>
      val port = p.asInstanceOf[MemPortIO]
      port.rdata := 0.U(port.rdata.getWidth.W)
      port.ack := false.B
      port.status := 0.U(3.W)
    }
    v
  }
}

// ==================== 系统级视图（多模块 / 多块 / 多寄存器） ====================

/**
 * 系统级用户逻辑连接视图。
 *
 * 三级命名访问：
 * {{{
 *   val sysView = SystemRegView(sysMap, sysRegFile)
 *   sysView.module("uart").reg("ctrl").field("baud_div").value      // 模块 → 寄存器 → 字段
 *   sysView.module("uart").block("ctrl_regs").reg("ctrl")            // 模块 → 块 → 寄存器
 *   sysView.reg("ctrl")                                              // 全系统平铺（寄存器名全局唯一）
 *   sysView.module("uart").regs                                      // 模块内所有寄存器
 * }}}
 */
class SystemRegView(sysMap: SystemMap, flatView: RegView) {
  /** 全系统平铺视图（寄存器名全局唯一时可用） */
  def reg(name: String): RegHandle = flatView.reg(name)
  def apply(name: String): RegHandle = flatView.reg(name)
  def names: Seq[String] = flatView.names

  /** 模块句柄 */
  def module(name: String): ModuleRegHandle =
    new ModuleRegHandle(sysMap.moduleByName(name), flatView)

  def modules: Seq[ModuleRegHandle] = sysMap.modules.map(ma => new ModuleRegHandle(ma, flatView))
}

/** 模块级句柄：模块 → 寄存器（或 → 块 → 寄存器） */
class ModuleRegHandle(ma: ModuleAllocation, flatView: RegView) {
  def name: String = ma.module.name
  def baseAddress: BigInt = ma.baseAddress
  def sizeBytes: BigInt = ma.sizeBytes

  /** 模块内寄存器（平铺查找，寄存器名须在模块内唯一） */
  def reg(name: String): RegHandle = flatView.reg(name)
  def apply(name: String): RegHandle = reg(name)

  /** 模块内所有寄存器（按块分组） */
  def regs: Seq[RegHandle] = ma.allRegs.map(a => flatView.reg(a.reg.name))

  /** 寄存器块句柄 */
  def block(name: String): BlockRegHandle =
    ma.regBlocks.find(_.block.name == name).map(b => new BlockRegHandle(b, this))
      .getOrElse(sys.error(s"reg block '$name' not found in module '$name', available: " +
        ma.regBlocks.map(_.block.name).mkString(", ")))

  def blocks: Seq[BlockRegHandle] = ma.regBlocks.map(b => new BlockRegHandle(b, this))

  /** 存储器块句柄 */
  def memBlock(name: String): MemBlockHandle =
    ma.memBlocks.find(_.block.name == name).map(b => new MemBlockHandle(b))
      .getOrElse(sys.error(s"mem block '$name' not found in module '$name'"))
}

/** 寄存器块级句柄：块 → 寄存器 */
class BlockRegHandle(rb: RegBlockAllocation, mod: ModuleRegHandle) {
  def name: String = rb.block.name
  def baseAddress: BigInt = rb.baseAddress
  def reg(name: String): RegHandle = mod.reg(name)
  def apply(name: String): RegHandle = reg(name)
  def regs: Seq[RegHandle] = rb.regs.map(a => mod.reg(a.reg.name))
}

/** 存储器块级句柄（描述性：外部 SRAM 经 io.memPorts 挂接） */
class MemBlockHandle(mb: MemBlockAllocation) {
  def name: String = mb.block.name
  def baseAddress: BigInt = mb.baseAddress
  def mems: Seq[MemAllocation] = mb.mems
}

object SystemRegView {
  /** 简单总线版 */
  def apply(sysMap: SystemMap, top: SystemRegFileTop): SystemRegView = {
    val v = new SystemRegView(sysMap, new RegView(sysMap.flatMap, top.io.user))
    top.io.memPorts.elements.foreach { case (_, p) =>
      val port = p.asInstanceOf[MemPortIO]
      port.rdata := 0.U(port.rdata.getWidth.W)
      port.ack := false.B
      port.status := 0.U(3.W)
    }
    v
  }

  /** AXI-Lite 版 */
  def apply(sysMap: SystemMap, top: SystemAxiLiteRegFile): SystemRegView = {
    val v = new SystemRegView(sysMap, new RegView(sysMap.flatMap, top.io.user))
    top.io.memPorts.elements.foreach { case (_, p) =>
      val port = p.asInstanceOf[MemPortIO]
      port.rdata := 0.U(port.rdata.getWidth.W)
      port.ack := false.B
      port.status := 0.U(3.W)
    }
    v
  }

  /** 自定义 user Record（透传版） */
  def apply(sysMap: SystemMap, top: SystemRegFileTop, user: RegUserRecord): SystemRegView =
    new SystemRegView(sysMap, new RegView(sysMap.flatMap, user))
}
