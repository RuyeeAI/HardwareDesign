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
  def roValue: UInt = fieldInput(reg.core.roValue, name)
  /** W1C/RC 字段置位端口（电平，或 1） */
  def hwSet: UInt = fieldInput(reg.core.hwSet, name)
  /** W1S/RS 字段清除端口（电平，与 0） */
  def hwClr: UInt = fieldInput(reg.core.hwClr, name)
  /** W1T 字段翻转端口（电平，异或） */
  def hwTog: UInt = fieldInput(reg.core.hwTog, name)
  /** RW 字段硬件直写数据端口（配合 reg.hwWrEn） */
  def hwWrData: UInt = fieldInput(reg.core.hwWrData, name)

  private def fieldInput(rec: Record, fname: String): UInt =
    rec.elements
      .getOrElse(fname,
        sys.error(s"field '$fname' of register '${reg.name}' has no such hardware input " +
          "(check access type / use the right accessor)"))
      .asInstanceOf[UInt]
}

class RegHandle(val name: String, val core: RegCoreIO, val alloc: RegAllocation) {
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

  def wrEn: Bool = core.wrEn
  def wrData: UInt = core.wrData
  def rdEn: Bool = core.rdEn
  def rdData: UInt = core.rdData
  def value: UInt = core.value
  /** RW 硬件直写使能（寄存器级） */
  def hwWrEn: Bool = core.hwWrEn
}

class RegView(map: RegFileMap, user: RegUserRecord) {
  // 用户侧输入默认置 0 / false（父模块作用域内的连接，满足 Chisel "sink not fully
  // initialized" 检查；用户随后对字段的显式连接会覆盖这些默认值）。
  // 注意：不能用 DontCare —— hwWrEn/hwSet 等会被硬件逻辑消费，DontCare 在仿真中
  // 表现为 x，会随机改写寄存器内容。
  map.regs.foreach { a =>
    val core = user.elements(a.reg.name).asInstanceOf[RegCoreIO]
    core.hwWrEn := false.B
    core.roValue.elements.foreach  { case (_, v) => v := 0.U(v.getWidth.W) }
    core.hwSet.elements.foreach    { case (_, v) => v := 0.U(v.getWidth.W) }
    core.hwClr.elements.foreach    { case (_, v) => v := 0.U(v.getWidth.W) }
    core.hwTog.elements.foreach    { case (_, v) => v := 0.U(v.getWidth.W) }
    core.hwWrData.elements.foreach { case (_, v) => v := 0.U(v.getWidth.W) }
  }

  private val cores: Map[String, RegCoreIO] =
    map.regs.map(a => a.reg.name -> user.elements(a.reg.name).asInstanceOf[RegCoreIO]).toMap
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
