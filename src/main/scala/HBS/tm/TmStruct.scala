package HBS.tm

import BaseCbb.data.GenBundle
import chisel3._
import chisel3.util.log2Ceil

class PacketEnqInfo (tmPar: TmParam) extends GenBundle{
  val vld     = Bool()
  val sop_ptr = UInt(tmPar.HbsPar.IbPtrW.W)
  val Psize   = UInt(tmPar.HbsPar.PktSizeW.W)
  val OqIndex = UInt(log2Ceil(tmPar.HbsPar.OqNumPerPp).W)
}