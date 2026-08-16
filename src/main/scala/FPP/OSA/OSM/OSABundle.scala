package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenBundle

// ===========================================================================
// Interface bundles — mirror docs/OSA.md §4
// ===========================================================================

/** MAC input interface: 20 segments x 8B per cycle (docs §4.1).
 *  `data` is 64-bit per segment (8B); `portId` per segment is supplied by
 *  the MAC (lane port granularity: segments of the 8x200G / 4x400G /
 *  2x800G / 1x1.6T ports are multiplexed on the lane and tagged with their
 *  port). NOTE: docs §4.1 shows UInt(8.W) per segment which is a typo —
 *  8B per segment requires 64 bits.
 */
class InterfaceMacOsa(config: OSAConfig) extends GenBundle {
  val data  = Vec(config.segmentsPerCycle, UInt(64.W))
  val valid = Vec(config.segmentsPerCycle, Bool())
  val sop   = Vec(config.segmentsPerCycle, Bool())
  val eop   = Vec(config.segmentsPerCycle, Bool())
  val err   = Vec(config.segmentsPerCycle, Bool())
  val portId= Vec(config.segmentsPerCycle, UInt(log2Ceil(config.portCount).W))
}

/** Tagged segment stream produced by SegDemux (docs §3.1). */
class TaggedSeg extends GenBundle {
  val data   = UInt(64.W)   // 8B segment
  val byteEn = UInt(8.W)    // byte enable (valid on packet tail)
  val portId = UInt(3.W)
  val slotId = UInt(2.W)    // context slot within port (0..2)
  val sop    = Bool()
  val eop    = Bool()
  val err    = Bool()
  val drop   = Bool()       // gated off buffer (ctx-full / SOP-overflow drop)
  val valid  = Bool()
}

/** New-packet window dispatched to the PPRS bank (docs §3.1). */
class NewPacketWindow extends GenBundle {
  val portId   = UInt(3.W)
  val slotId   = UInt(2.W)
  val first32B = UInt(256.W)
  val sopPos   = UInt(5.W)
}

/** PPRS result (docs §3.3). */
class PriResult extends GenBundle {
  val portId    = UInt(3.W)
  val slotId    = UInt(2.W)
  val orgQindex = UInt(4.W)
  val src       = UInt(3.W)   // 0=default,1=tcam,2=vlan,3=dscp,4=opaque
  val err       = Bool()
}

/** Packet descriptor (docs §3.8 / §4.2). */
class PacketDesc extends GenBundle {
  val portId    = UInt(3.W)
  val pktId     = UInt(8.W)
  val macHeader = UInt(64.W)
  val byteCount = UInt(16.W)
  val segCount  = UInt(16.W)
  val bufBase   = UInt(17.W)
  val orgQindex = UInt(4.W)
  val priClass  = UInt(2.W)
  val err       = Bool()
}

/** 96B output unit — belongs to exactly one packet (no packing, docs §4.3). */
class Osa96bUnit extends GenBundle {
  val data   = Vec(12, UInt(8.W))
  val valid  = Vec(12, Bool())
  val byteEn = Vec(12, UInt(8.W))
  val sop    = Bool()
  val eop    = Bool()
  val error  = Bool()
}

/** Output beat: 2 x 96B, same port, <= 1 SOP (docs §4.3). */
class CellOutputBundle(config: OSAConfig) extends GenBundle {
  val units  = Vec(config.outUnitsPerBeat, new Osa96bUnit)
  val portId = UInt(3.W)
  val lbo    = Bool()
  val obi    = Valid(new OutOfBandInfo)
}

/** Out-of-band info (docs §4.3). */
class OutOfBandInfo extends GenBundle {
  val macHeader = UInt(64.W)
  val portId    = UInt(3.W)
  val pktId     = UInt(8.W)
  val orgQindex = UInt(4.W)
  val priClass  = UInt(2.W)
  val byteCount = UInt(16.W)
  val timestamp = UInt(32.W)
}

/** Backpressure to MAC: bp(port)(pfc) (docs §4.4). */
class BackpressureOutput(config: OSAConfig) extends GenBundle {
  val bp = Vec(config.portCount, Vec(config.maxPfcPriority, Bool()))
}

/** Priority class -> PFC priority map (docs §4.4). */
class PfcPriMap extends GenBundle {
  val lossyLowPfcp     = UInt(3.W)
  val lossyHighPfcp    = UInt(3.W)
  val losslessLowPfcp  = UInt(3.W)
  val losslessHighPfcp = UInt(3.W)
}

/** Per-port admission thresholds (docs §3.9). */
class PortThresholds extends GenBundle {
  val lossyLow    = UInt(16.W)
  val lossyHigh   = UInt(16.W)
  val lossless    = UInt(16.W)
  val hysteresis  = UInt(16.W)
}

/** Assembly-complete event (docs §3.5). */
class PktAssemblyDone extends GenBundle {
  val portId    = UInt(3.W)
  val slotId    = UInt(2.W)
  val macHeader = UInt(64.W)
  val byteCount = UInt(16.W)
  val segCount  = UInt(16.W)
  val orgQindex = UInt(4.W)
  val priClass  = UInt(2.W)
  val err       = Bool()
  val tooSmall  = Bool()
}

/** Buffer rollback info on drop (docs §3.9). */
class RollbackInfo extends GenBundle {
  val portId   = UInt(3.W)
  val segCount = UInt(16.W)
}

/** Read data returned to CellAsm (docs §3.10). */
class BufReadData extends GenBundle {
  val data   = UInt(64.W)
  val byteEn = UInt(8.W)
  val isSOP  = Bool()
  val isEOP  = Bool()
  val err    = Bool()
  val valid  = Bool()
  val portId = UInt(3.W)
  val pktId  = UInt(8.W)
}

/** One full output beat of read data (24 segments). */
class BufReadDataVec(config: OSAConfig) extends GenBundle {
  val segs = Vec(config.outSegPerBeat, new BufReadData)
}

/** Bank write request (docs §3.7). */
class BankWrReq(config: OSAConfig) extends GenBundle {
  val we   = Bool()
  val addr = UInt(config.bankRowAddrW.W)
  val data = UInt(64.W)
  val eop  = Bool()
  val ben  = UInt(8.W)
}

/** Bank read request (docs §3.7). */
class BankRdReq(config: OSAConfig) extends GenBundle {
  val addr = UInt(config.bankRowAddrW.W)
  val tag  = UInt(8.W)
}

/** Bank read response (docs §3.7). */
class BankRdResp(config: OSAConfig) extends GenBundle {
  val tag    = UInt(8.W)
  val data   = UInt(64.W)
  val uecErr = Bool()
}
