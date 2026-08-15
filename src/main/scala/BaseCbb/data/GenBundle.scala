package BaseCbb.data

import chisel3._

// Common base classes recovered from the deleted utils/GeneratorLib.scala.
// Many modules (FPP/*, BaseCbb/memory, BaseCbb/fifo, BaseCbb/arbiter, ...)
// depend on these symbols. Only the still-referenced ones are restored here:
// GenModule / GenBundle / fldAttr.

/** Generic Module base class */
class GenModule extends Module

/** Field attribute: description / reset value / expand-array flag */
case class fldAttr(Desc: String, ResetValue: Long = 0L, ExpandArr: Boolean = false)

/** Generic Bundle base class with per-field annotations (Attr) */
class GenBundle extends Bundle {
  var Attr: Map[Data, fldAttr] = Map()
}
