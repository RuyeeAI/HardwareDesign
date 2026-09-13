
package HBS.swf.common

import BaseCbb.data.GenBundle
import chisel3._
import chisel3.util._
class InterfaceISReqBundle (p:SwfParams) extends GenBundle{
  val tp         = UInt(p.DTpW.W)
  val data       = UInt(p.DataCellSize.W)
  val cell_id    = UInt(6.W) // Number of Cell ID in the packets
  val pkt_id     = UInt(p.PktIdW.W)
  val ob_num     = UInt(p.NObW.W)
  val target_lane= UInt(6.W)
  val err        = Bool()
}

class InterfaceISReq(p:SwfParams) extends GenBundle{
  val data = Valid(UInt((new InterfaceISReqBundle(p)).getWidth.W))
}



class InterfaceISAck (p:SwfParams) extends GenBundle{
  val ack_tp     = UInt(p.DTpW.W)
  val ack_ob_num = UInt(p.NObW.W)
}

class InterfaceIS (p:SwfParams) extends GenBundle{
  val req = Flipped(ValidIO(new InterfaceISReq(p)))
  val ack = ValidIO(new InterfaceISAck(p))
}

class SwfInterfaceIS(p:SwfParams) extends GenBundle{
  val req = Input(new InterfaceISReq(p))
  val ack = ValidIO(new InterfaceISAck(p)) //Output
}


class SwfInterfaceID(p:SwfParams) extends GenBundle{
  val req = Input(new InterfaceIDReq(p))
  val ack = ValidIO(new InterfaceIDAck(p))
}

class InterfaceIDReq(p:SwfParams) extends GenBundle{
  val data = Valid(UInt((new InterfaceIDReqBundle(p)).getWidth.W))
}

class InterfaceID (p:SwfParams) extends GenBundle{
  val valid = Input(Bool())
  val sp    = Input(UInt(p.NSpW.W))// need check whether it's needed
  val tp    = Input(UInt(p.DTpW.W))
  val dscr  = Input(UInt(p.CtDscSize.W))
  val ptr   = Input(UInt(p.PtrWidth.W))
  val ack   = Output(Bool())
}
class InterfaceIDReqBundle (p:SwfParams) extends GenBundle{
  val sp    = Input(UInt(p.NSpW.W))// need check whether it's needed
  val tp    = Input(UInt(p.DTpW.W))
  val dscr  = Input(UInt(p.CtDscSize.W))
  val ptr   = Input(UInt(p.PtrWidth.W))
  val target_lane = Input(UInt(4.W))
}
class InterfaceIDAck(p:SwfParams) extends GenBundle{
  val rsv = UInt(1.W)
}

class InterfaceSOBundle(p:SwfParams) extends GenBundle{
  val tp         = Input(UInt(p.DTpW.W))
  val data: UInt = Input(UInt(p.DataCellSize.W))
  val cell_id    = Input(UInt(6.W)) // Number of Cell ID in the packets
  val pkt_id     = Input(UInt(p.PktIdW.W))
  val err        = Input(Bool())
  //val ack        = Output(Bool())
  //val ack_tp     = Output(UInt(p.DTpW.W))
  //val ack_ob_num = Output(UInt(p.NObW.W))
}

class InterfaceSO (p:SwfParams) extends GenBundle{
  val data = Valid(UInt((new InterfaceSOBundle(p)).getWidth.W))
}
class InterfaceIb2SwfData (p:SwfParams) extends InterfaceIS(p)

class InterfaceSwf2ObgData (p:SwfParams) extends GenBundle{

}
class InterfaceSwf2D2DgData (p:SwfParams) extends GenBundle{

}


class InterfaceIb2SwfCtDsc(p:SwfParams) extends InterfaceID(p)


class InterfaceSwf2ObgDsc(p:SwfParams) extends GenBundle{
  val data = UInt(p.CtDscSize.W)
}

class InterfaceDsc2SwfSafDsc(p:SwfParams) extends GenBundle{
  val data = UInt(p.SafDscSize.W)
}

class InterfaceSwf2D2DgDsc(p:SwfParams) extends GenBundle{
  val data = UInt(p.CtDscSize.W)
}

class IbSwfIntf(p:SwfParams) extends GenBundle{
  val ib_swf_data         = Input (Vec(p.IbSwfDp     ,  Valid(new InterfaceISReq(p))))
  val ib_swf_data_ack     = Output(Vec(p.IbSwfDp * 2 ,  Valid(new InterfaceISAck(p)))) // This is from OB to IB controllor, should not pass SWF path again.
  val ib_swf_ct_dsc       = Input (Vec(p.IbSwfDsc    ,  Valid(new InterfaceIDReq(p))))
  val ib_swf_ct_dsc_ack   = Output(Vec(p.IbSwfDsc * 2,  Valid(new InterfaceIDAck(p))))
}

class SwfObgIntf(p:SwfParams) extends GenBundle{
  val data         = Output(Vec(p.SfuOutDp  , new InterfaceSO(p)))
  val dsc          = Output(Vec(p.SwfObgCp  , new InterfaceIDReq(p)))
}

class DscSwfIntf(p:SwfParams) extends GenBundle{
  val saf_dsc     = Input (Vec(p.DscSwfDsc    , Valid(new InterfaceIDReq(p))))
  val saf_dsc_ack = Output(Vec(p.DscSwfDsc * 2, Valid(new InterfaceIDAck(p))))
}