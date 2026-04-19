package BaseCbb.RegCbb.dsl

import scala.collection.mutable.ArrayBuffer

/** Memory/SRAM definition */
case class MemoryDef(
  name: String,
  depth: Int,
  dataWidth: Int,
  memType: MemoryAccessType = MemoryAccessType.SP,
  baseAddress: BigInt = 0,
  description: String = ""
) {
  require(depth > 0, s"Memory $name: depth must be > 0")
  require(dataWidth > 0 && dataWidth <= 256, s"Memory $name: dataWidth must be 1-256")
  require(baseAddress >= 0, s"Memory $name: baseAddress must be >= 0")

  val addrWidth: Int = scala.math.ceil(scala.math.log(depth) / scala.math.log(2)).toInt
  val byteSize: BigInt = BigInt(depth) * BigInt(dataWidth) / 8
}

/** Builder for Memory definition */
class MemoryBuilder(memName: String) {
  private var _depth: Int = 64
  private var _dataWidth: Int = 32
  private var _memType: MemoryAccessType = MemoryAccessType.SP
  private var _baseAddress: BigInt = 0
  private var _desc: String = ""

  def depth(d: Int): this.type = { _depth = d; this }
  def dataWidth(w: Int): this.type = { _dataWidth = w; this }
  def width(w: Int): this.type = dataWidth(w)

  def sp(): this.type = { _memType = MemoryAccessType.SP; this }
  def tp(): this.type = { _memType = MemoryAccessType.TP; this }

  def baseAddress(addr: BigInt): this.type = { _baseAddress = addr; this }
  def addr(addr: BigInt): this.type = baseAddress(addr)

  def desc(d: String): this.type = { _desc = d; this }

  def build(): MemoryDef = {
    require(_depth > 0, s"Memory ${memName}: depth must be > 0")
    MemoryDef(memName, _depth, _dataWidth, _memType, _baseAddress, _desc)
  }
}

object MemoryDef {
  def apply(name: String)(block: MemoryBuilder => Unit): MemoryDef = {
    val builder = new MemoryBuilder(name)
    block(builder)
    builder.build()
  }

  def sp(name: String, depth: Int = 64, dataWidth: Int = 32, baseAddress: BigInt = 0, desc: String = "")
         (block: MemoryBuilder => Unit = _ => {}): MemoryDef =
    MemoryDef(name) { b =>
      b.sp().depth(depth).dataWidth(dataWidth).baseAddress(baseAddress).desc(desc)
      block(b)
    }

  def tp(name: String, depth: Int = 64, dataWidth: Int = 32, baseAddress: BigInt = 0, desc: String = "")
         (block: MemoryBuilder => Unit = _ => {}): MemoryDef =
    MemoryDef(name) { b =>
      b.tp().depth(depth).dataWidth(dataWidth).baseAddress(baseAddress).desc(desc)
      block(b)
    }
}
