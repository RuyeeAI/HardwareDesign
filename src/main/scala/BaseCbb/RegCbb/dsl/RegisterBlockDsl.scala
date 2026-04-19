package BaseCbb.RegCbb.dsl

import scala.collection.mutable.ArrayBuffer

/** A register definition - a collection of fields at a specific address */
case class RegDef(
  name: String,
  offset: BigInt,
  fields: Seq[RegFieldDef],
  description: String = "",
  group: Option[String] = None
) {
  val totalBits: Int = fields.map(_.bitWidth).sum
  val byteSize: Int = (totalBits + 7) / 8
}

/** A register block definition - collection of registers and memories */
case class RegBlockDef(
  name: String,
  regBaseAddress: BigInt,
  memBaseAddress: BigInt,
  registers: Seq[RegDef],
  memories: Seq[MemoryDef],
  description: String = "",
  deviceName: String = "device"
) {
  val totalRegByteSize: BigInt = {
    if (registers.isEmpty) 0
    else registers.map(r => r.offset + r.byteSize).max + 1
  }

  val totalMemByteSize: BigInt = {
    if (memories.isEmpty) 0
    else memories.map(m => m.baseAddress + m.byteSize).max + 1
  }
}

/** Builder for a single register */
class RegisterBuilder(regName: String) {
  private val fieldDefs = ArrayBuffer[RegFieldDef]()
  private var _desc: String = ""
  private var _groupName: Option[String] = None

  def field(f: RegFieldDef): this.type = {
    fieldDefs += f; this
  }

  def desc(d: String): this.type = { _desc = d; this }
  def group(g: String): this.type = { _groupName = Some(g); this }

  def build(offset: BigInt): RegDef =
    RegDef(regName, offset, fieldDefs.toSeq, _desc, _groupName)
}

/** Builder for register block */
class RegBlockBuilder(blockName: String) {
  private val registerDefs = ArrayBuffer[RegDef]()
  private val memoryDefs = ArrayBuffer[MemoryDef]()
  private var _desc: String = ""
  private var _device: String = blockName
  private var _regBaseAddr: BigInt = 0
  private var _memBaseAddr: BigInt = 0

  def name(n: String): this.type = { _device = n; this }

  def baseAddress(addr: BigInt): this.type = { _regBaseAddr = addr; this }

  def memBaseAddress(addr: BigInt): this.type = { _memBaseAddr = addr; this }

  def desc(d: String): this.type = { _desc = d; this }
  def device(d: String): this.type = { _device = d; this }

  def reg(name: String)(block: RegisterBuilder => Unit): this.type = {
    val builder = new RegisterBuilder(name)
    block(builder)
    registerDefs += builder.build(0)  // offset assigned later by allocator
    this
  }

  def mem(name: String)(block: MemoryBuilder => Unit): this.type = {
    val builder = new MemoryBuilder(name)
    block(builder)
    memoryDefs += builder.build().copy(baseAddress = _memBaseAddr)
    this
  }

  def build(): RegBlockDef =
    RegBlockDef(blockName, _regBaseAddr, _memBaseAddr, registerDefs.toSeq, memoryDefs.toSeq, _desc, _device)
}

object RegBlock {
  def apply(name: String)(block: RegBlockBuilder => Unit): RegBlockDef = {
    val builder = new RegBlockBuilder(name)
    block(builder)
    builder.build()
  }
}
