package HBS.top

import BaseCbb.data.GenParam
import chisel3.util._

class HbsParams extends GenParam{
  val PpNum            = 24
  val IbSize           = 80    // in MB
  val IbNum            = 4     // Per Die
  val CellSize         = 256*8
  val MaxPktSize       = 9600
  val NwPortNumPerLane = 8
  val PpLaneNum        = 3
  val SpPortNumPerLane = 1
  val LbPortNumPerLane = 1
  val OqNumPerNwPort   = 12
  val OqNumPerSpPort   = 64
  val OqNumperLbPort   = 64

  val OqNumPerlane     = NwPortNumPerLane*OqNumPerNwPort + SpPortNumPerLane*OqNumPerSpPort + LbPortNumPerLane * OqNumperLbPort
  val OqNumPerPp       = OqNumPerlane*PpLaneNum

  val IbPtrW           = log2Ceil(IbSize*1024*1024*8/CellSize)
  val PktSizeW         = log2Ceil(MaxPktSize)
}
