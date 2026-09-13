package HBS.swf.common

import BaseCbb.data.GenParam
import chisel3.util.log2Ceil

class SwfParams extends GenParam{
  val NIB   = 2
  Desc+=("NIB"->"Number of IB instances per die")
  val NOBG  = 4
  Desc+=("NOBG"->"Number of OBG Instance per die")
  val ND2DG = 4
  Desc+=("ND2DG"->"Number of D2DG Instance per die")
  val NOB   = 3
  Desc+=("NOB"->"Number of OB instance per OBG")
  val ND2D  = 3
  Desc+=("ND2D"->"Number of D2D instance per D2DG")
  val DataCellSize = 2048
  Desc+=("DataCellSize"->"Packet data cell size")
  val CtDscSize = 100
  Desc+=("CtDscSize"->"CT descriptor size")
  val SafDscSize = 200
  Desc+=("SafDscSize"->"Store And Forward descriptor size")
  val IbSwfDp = 20
  Desc+=("IbSwfDp"->"Number of datapaths per IB to SWF")
  val RoutSfuSwiDp = 20
  Desc+=("RoutSfuSwiDp"->"Number of datapaths from Routing SFU to Switching SFU")
  val IbSwfDsc  = 6
  Desc+=("IbSwfDsc"->"Number of CT descriptor path per IB to SWF")
  val DscSwfDsc = 8
  Desc+=("DscSwfDsc"->"Number of SAF descriptor path per DSC to SWF")
  val RoutSfuSwiCtCp = 3
  Desc+=("RoutSfuSwiCtCp"->"Number of CT descriptor paths from Routing SFU to each of the Switching SFUs")
  val RoutSfuSwiSafCp = 4
  Desc+=("RoutSfuSwiSafCp"->"Number of SAF descriptor paths from ROuting SFU to each of the Switching SFUs")
  val SfuOutDp = 12
  Desc+=("SfuOutDp"->"Number of data paths from switching SFU to each of the OBGs")
  val SwfObgCp = 6
  Desc+=("SwfObgCp"->"Number of descriptor paths from switching SFU to each of the OBGs")
  val SwfD2DgCp = 6
  Desc+=("SwfD2DgCp"->"Number of descriptor paths from switching SFU to each of the D2DGs")
  val MiddleFifoDepth = 16
  Desc+=("MiddleFifoDepth"->"The depth of the FIFOs between the two middle SFUs which hold the data cells and descriptors")
  val MiddleFifoThHigh = 12
  Desc+=("MiddleFifoThHigh"->"Middle FIFO threshold high determine asserting BP")
  val MiddleFifoThLow  = 8
  Desc+=("MiddleFifoThLow"->"Middle FIFO threshold low determine de-asserting BP")
  val DTpW             = 5
  Desc+=("DTpW"->"Target port width")
  val PktIdW           = 10
  Desc+=("PktIdW"->"Width of packet ID through SWF")
  val NObW             = 5
  Desc+=("NObW"->"ID of OB")
  val NSpW             = 10
  Desc+=("NSpW"->"the source port ID width")
  val IbSize: Int = 64*1024*1024
  val PtrWidth: Int = log2Ceil(IbSize/256)
  Desc+=("PtrWidth"->"the pointer width")
  val SfuDataBufDep = 64
  Desc+=("SfuDataBufDep"->"Data buffer depth in SFU")
 // val SfuRoutingSampling = List(0, 1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  4,  4,  4)


}
