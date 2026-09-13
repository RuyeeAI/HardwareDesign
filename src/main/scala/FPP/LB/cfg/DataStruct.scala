package FPP.LB.cfg

import BaseCbb.data.GenBundle
import chisel3._


/**
 * When the weight in the entry > random value, select
 * @param WeigthWidth
 * @param OffsetWidth
 */
class WcmpIndexTable(WeightWidth:Int, OffsetWidth:Int) extends GenBundle{
  val weight  = UInt(WeightWidth.W)
  val offset0 = UInt(OffsetWidth.W)
  val offset1 = UInt(OffsetWidth.W)
}

