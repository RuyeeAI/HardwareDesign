package FPP.OSA.PreParser

import BaseCbb.data.GenBundle
import chisel3._

// ============= TCAM Entry =============

class TcamEntry extends GenBundle {
  val dmacMask = UInt(48.W)
  val dmacValue = UInt(48.W)
  val smacMask = UInt(48.W)
  val smacValue = UInt(48.W)
  val priority = UInt(4.W)
  val valid = Bool()
}

object TcamEntry {
  def default = {
    val entry = Wire(new TcamEntry)
    entry.dmacMask := 0.U(48.W)
    entry.dmacValue := 0.U(48.W)
    entry.smacMask := 0.U(48.W)
    entry.smacValue := 0.U(48.W)
    entry.priority := 0.U(4.W)
    entry.valid := false.B
    entry
  }
}

// ============= Input/Output Bundles =============

class PreParserInput extends GenBundle {
  val data = UInt(256.W)    // 32 bytes × 8 bits
  val portId = UInt(4.W)    // Port ID (0-15)
  val valid = Bool()
}

class PreParserOutput extends GenBundle {
  val priority = UInt(4.W)
  val valid = Bool()
}

// ============= Internal Result Bundles =============

/** VLAN extraction result */
class VlanExtractResult extends Bundle {
  val vlanCount = UInt(2.W)       // Number of VLAN tags found (0-3)
  val vlanPrio = UInt(4.W)         // DEI + PRI from outermost VLAN
  val vlanVid = UInt(12.W)         // VID from outermost VLAN
  val hasOpaqueTag = Bool()
  val hasIp = Bool()
  val dscp = UInt(6.W)             // DSCP from IP header if present
}

/** OpaqueTag extraction result */
class OpaqueExtractResult extends Bundle {
  val isValid = Bool()
  val format = UInt(4.W)         // Format type
  val length = UInt(2.W)         // 0=4B, 1=8B (in 4B units)
  val priority = UInt(4.W)       // PRI (no DEI in OpaqueTag)
}

object OpaqueExtractResult {
  def invalid = {
    val r = Wire(new OpaqueExtractResult)
    r.isValid := false.B
    r.format := 0.U(4.W)
    r.length := 0.U(2.W)
    r.priority := 0.U(4.W)
    r
  }
}

/** DSCP extraction result */
class DscpExtractResult extends Bundle {
  val isValid = Bool()
  val dscp = UInt(6.W)
}

/** TCAM match result */
class TcamMatchResult extends Bundle {
  val hit = Bool()
  val priority = UInt(4.W)
}

/** Priority selection result */
class PriorityResult extends Bundle {
  val priority = UInt(4.W)
  val source = UInt(3.W)   // 0=default, 1=tcam, 2=vlan, 3=dscp, 4=opaque
  val valid = Bool()
}

object PriorityResult {
  def default = {
    val r = Wire(new PriorityResult)
    r.priority := 0.U(4.W)
    r.source := 0.U(3.W)
    r.valid := false.B
    r
  }
}

// ============= LUT Data Bundles =============

/** VLAN Priority LUT entry (value only) */
class VlanPrioLutEntry extends GenBundle {
  val priority = UInt(4.W)
}

/** DSCP Priority LUT entry (value only) */
class DscpPrioLutEntry extends GenBundle {
  val priority = UInt(4.W)
}

/** OpaqueTag Priority LUT entry (value only) */
class OpaquePrioLutEntry extends GenBundle {
  val priority = UInt(4.W)
}