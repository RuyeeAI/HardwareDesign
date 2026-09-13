package HBS.swf.swf_top

import BaseCbb.data.GenModule
import HBS.swf.common._
import chisel3._

class SfuTop (p:SwfParams) extends GenModule{
  val io = IO(new Bundle{
    // number of NIB from each of IB
    val ib_swf_data     = Vec(p.NIB,Vec(p.IbSwfDp,new InterfaceIb2SwfData(p)))
    //Interface from SFU corner to each OBG
    val swf_obg_data    = Vec(p.NOBG,Vec(p.NOB,new InterfaceSwf2ObgData(p)))
    //Interface from SFU corner to each D2DG
    val swf_d2dg_data    = Vec(p.ND2DG,Vec(p.ND2D,new InterfaceSwf2D2DgData(p)))
    // CT DSC from each CT
    val ib_swf_ct_dsc   = Vec(p.NIB,Vec(p.IbSwfDsc,new InterfaceIb2SwfCtDsc(p)))
    val dsc_swf_saf_dsc = Vec(p.DscSwfDsc,new InterfaceDsc2SwfSafDsc(p))
    val swf_obg_dsc     = Vec(p.SwfObgCp,new InterfaceSwf2ObgDsc(p))
    val swf_d2dg_dsc    = Vec(p.SwfD2DgCp,new InterfaceSwf2D2DgDsc(p))
    // Data cell fill level in the VOQ memory
  })


}
