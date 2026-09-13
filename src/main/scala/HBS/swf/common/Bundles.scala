package HBS.swf.common

import BaseCbb.data.GenBundle
import chisel3._
import chisel3.util.{Valid, log2Ceil}



class SfuDataCfgBundle extends GenBundle{
  val spr_cnt_th = UInt(4.W)
  val wrr_weight = Vec(2,UInt(5.W))
}


class MidSfuCfgBundle extends GenBundle{
  val middle_fifo_th_high = UInt(5.W)
  val middle_fifo_th_low  = UInt(5.W)
  val spr_cnt_th          = UInt(4.W)
  val hl_wrr_weight       = Vec(2,UInt(5.W))
  val mid_nbl_wrr_weight  = Vec(2,UInt(5.W))
}

class SfuCorCfgBundle extends GenBundle{
  val data = new SfuDataCfgBundle
  val dsc  = new SfuDataCfgBundle
}

class SwfCfg extends GenBundle{
  val cor= Vec(4,new SfuDataCfgBundle)
  val mid= Vec(4,new MidSfuCfgBundle)
}

class VoqEnqReq(DstNum:Int) extends GenBundle{
  val vld = Bool()
  val dst = UInt(log2Ceil(DstNum).W)
}

class CorMidIntfBundle(p:SwfParams) extends GenBundle{
  val mid_cor_data_spr_pre_valid = Input(Vec(p.SfuOutDp*2       , Bool()))
  val mid_cor_data_spr_bp        = Input(Vec(p.SfuOutDp*2       , Bool()))
  val cor_mid_data_spr_pre_valid = Output(Vec(p.SfuOutDp*2      , Bool()))
  val cor_mid_data_spr_bp        = Output(Vec(p.SfuOutDp*2      , Bool()))
  val mid_cor_data               = Input(Vec(p.SfuOutDp*2       , new InterfaceISReq(p)))
  val cor_mid_data               = Output(Vec(p.SfuOutDp*2      , new InterfaceISReq(p)))

  val mid_cor_dsc_spr_pre_valid  = Input(Vec(p.SwfObgCp*2       , Bool()))
  val mid_cor_dsc_spr_bp         = Input(Vec(p.SwfObgCp*2       , Bool()))
  val cor_mid_dsc_spr_pre_valid  = Output(Vec(p.SwfObgCp*2       , Bool()))
  val cor_mid_dsc_spr_bp         = Output(Vec(p.SwfObgCp*2      , Bool()))
  val cor_mid_dsc                = Output(Vec(p.SwfObgCp*2      , new InterfaceIDReq(p)))
  val mid_cor_dsc                = Input(Vec(p.SwfObgCp*2       , new InterfaceIDReq(p)))
}

class RoutSfuIntfBundle(p:SwfParams) extends GenBundle{
  val data    = Vec(p.RoutSfuSwiDp,    new SwfInterfaceIS(p))
  val ct_dsc  = Vec(p.RoutSfuSwiCtCp,  new SwfInterfaceID(p))
  val saf_dsc = Vec(p.RoutSfuSwiSafCp, new SwfInterfaceID(p))
}

class MidMidIntfBundle (p:SwfParams) extends GenBundle{
  val nbl_mid_data                = Input(Vec(p.SfuOutDp*2       , new InterfaceISReq(p)))
  val mid_nbl_data                = Output(Vec(p.SfuOutDp*2      , new InterfaceISReq(p)))
  val nbl_mid_data_bp             = Input(Vec(p.SfuOutDp*2       , Bool()))
  val mid_nbl_data_bp             = Output(Vec(p.SfuOutDp*2      , Bool()))

  val nbl_mid_dsc                 = Input(Vec(p.SwfObgCp*2       , new InterfaceIDReq(p)))
  val mid_nbl_dsc                 = Output(Vec(p.SwfObgCp*2      , new InterfaceIDReq(p)))
  val nbl_mid_dsc_bp              = Input(Vec(p.SwfObgCp*2       , Bool()))
  val mid_nbl_dsc_bp              = Output(Vec(p.SwfObgCp*2      , Bool()))
}

