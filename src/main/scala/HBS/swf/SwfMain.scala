package HBS.swf

import HBS.swf.common.{BusMatrix, SwfParams}
import HBS.swf.sfu_corner.SfuCorner
import HBS.swf.sfu_mid.SfuMid
import HBS.swf.sfu_routing.SfuRouting
import HBS.swf.swf_top.SwfCore
import circt.stage.ChiselStage
import chisel3._


object SwfMain extends App{
  ChiselStage.emitSystemVerilogFile(
    //    new WRR(2,4),
//    new SfuCorner(new SwfParams),
//    new BusMatrix(UInt(32.W),12,24),
    new SwfCore(new SwfParams),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )
}

