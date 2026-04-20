package BaseCbb.RegCbb.dsl

import chisel3._
import scala.collection.mutable.ArrayBuffer

/** Access types for register fields */
sealed trait AccessType
object AccessType {
  case object RO extends AccessType  // Read-only
  case object WO extends AccessType  // Write-only
  case object RW extends AccessType  // Read-write
  case object RC extends AccessType  // Read-to-clear
  case object RS extends AccessType  // Read-to-set
}

/** Write action types for register fields */
sealed trait WriteAction
object WriteAction {
  case object Normal extends WriteAction       // Direct write
  case object OneToClear extends WriteAction    // Write 1 to clear
  case object OneToSet extends WriteAction      // Write 1 to set
  case object OneToToggle extends WriteAction    // Write 1 to toggle
  case object ClearOnRead extends WriteAction   // Auto-clear after read
}

/** Memory access types */
sealed trait MemoryAccessType
object MemoryAccessType {
  case object SP extends MemoryAccessType  // Single-port SRAM
  case object TP extends MemoryAccessType  // Two-port SRAM
}

/** Field definition */
case class RegFieldDef(
  name: String,
  bitWidth: Int,
  access: AccessType = AccessType.RW,
  resetValue: BigInt = 0,
  description: String = "",
  writeAction: WriteAction = WriteAction.Normal,
  enumerations: Map[BigInt, (String, String)] = Map()
) {
  require(bitWidth > 0, s"Field $name: bitWidth must be > 0")
  require(bitWidth <= 256, s"Field $name: bitWidth must be <= 256")
  require(resetValue >= 0,
    s"Field $name: resetValue $resetValue must be >= 0")
  // For resetValue range, use BigInt for >64bit widths
  if (bitWidth <= 64) {
    require(resetValue < (BigInt(1) << bitWidth),
      s"Field $name: resetValue $resetValue out of range for $bitWidth bits")
  }
}

/** Fluent DSL for building a field */
class FieldBuilder {
  private var _name: String = ""
  private var _width: Int = 1
  private var _access: AccessType = AccessType.RW
  private var _reset: BigInt = 0
  private var _desc: String = ""
  private var _writeAct: WriteAction = WriteAction.Normal
  private var _enums: Map[BigInt, (String, String)] = Map()

  def named(n: String): this.type = { _name = n; this }
  def width(w: Int): this.type = { _width = w; this }
  def bits(w: Int): this.type = width(w)

  def ro(): this.type = { _access = AccessType.RO; this }
  def wo(): this.type = { _access = AccessType.WO; this }
  def rw(): this.type = { _access = AccessType.RW; this }
  def rc(): this.type = { _access = AccessType.RC; this }
  def rs(): this.type = { _access = AccessType.RS; this }

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
    require(_name.nonEmpty, "Field must have a name")
    RegFieldDef(_name, _width, _access, _reset, _desc, _writeAct, _enums)
  }
}

object RegField {
  def apply(name: String, width: Int = 1)(block: FieldBuilder => Unit): RegFieldDef = {
    val builder = new FieldBuilder
    builder.named(name).width(width)
    block(builder)
    builder.build()
  }

  def ro(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "")
           (block: FieldBuilder => Unit = _ => {}): RegFieldDef =
    RegField(name, width) { b => b.ro().reset(reset).desc(desc); block(b) }

  def rw(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "")
           (block: FieldBuilder => Unit = _ => {}): RegFieldDef =
    RegField(name, width) { b => b.rw().reset(reset).desc(desc); block(b) }

  def wo(name: String, width: Int = 1, desc: String = "")
           (block: FieldBuilder => Unit = _ => {}): RegFieldDef =
    RegField(name, width) { b => b.wo().desc(desc); block(b) }

  def rc(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "")
           (block: FieldBuilder => Unit = _ => {}): RegFieldDef =
    RegField(name, width) { b => b.rc().reset(reset).desc(desc); block(b) }

  def rs(name: String, width: Int = 1, reset: BigInt = 0, desc: String = "")
           (block: FieldBuilder => Unit = _ => {}): RegFieldDef =
    RegField(name, width) { b => b.rs().reset(reset).desc(desc); block(b) }
}
