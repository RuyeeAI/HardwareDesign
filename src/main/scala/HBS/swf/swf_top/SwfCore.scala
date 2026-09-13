package HBS.swf.swf_top

import BaseCbb.data.GenModule
import HBS.swf.common._
import HBS.swf.sfu_corner._
import HBS.swf.sfu_mid._
import HBS.swf.sfu_routing._
import chisel3._

class SwfCore (p:SwfParams) extends GenModule{
  val clk   = IO(Input(Clock()))
  val rst_n = IO(Input(Bool()))
  val io    = IO(new Bundle{
    val ib0_swf_intf        = new IbSwfIntf(p)
    val ib1_swf_intf        = new IbSwfIntf(p)
    val ib2_swf_intf        = new IbSwfIntf(p)
    val ib3_swf_intf        = new IbSwfIntf(p)

    val swf_obg0_intf       = new SwfObgIntf(p)
    val swf_obg1_intf       = new SwfObgIntf(p)
    val swf_obg2_intf       = new SwfObgIntf(p)
    val swf_obg3_intf       = new SwfObgIntf(p)
    val swf_obg4_intf       = new SwfObgIntf(p)
    val swf_obg5_intf       = new SwfObgIntf(p)
    val swf_obg6_intf       = new SwfObgIntf(p)
    val swf_obg7_intf       = new SwfObgIntf(p)

    val dsc_swf_intf        = Vec(2,new DscSwfIntf(p))
    val cfg                 = Input(new SwfCfg)
  })

  val U_SFU_COR0 = Module(new SfuCorner(p))
  val U_SFU_COR1 = Module(new SfuCorner(p))
  val U_SFU_COR2 = Module(new SfuCorner(p))
  val U_SFU_COR3 = Module(new SfuCorner(p))

  val U_SFU_MID0 = Module(new SfuMid(p))
  val U_SFU_MID1 = Module(new SfuMid(p))
  val U_SFU_MID2 = Module(new SfuMid(p))
  val U_SFU_MID3 = Module(new SfuMid(p))

  val U_SFU_ROUT0 = Module(new SfuRouting(p))
  val U_SFU_ROUT1 = Module(new SfuRouting(p))

  U_SFU_ROUT0.ib_swf_intf(0) <> io.ib0_swf_intf
  U_SFU_ROUT0.ib_swf_intf(1) <> io.ib1_swf_intf
  U_SFU_ROUT0.dsc_swf_intf   <> io.dsc_swf_intf(0)
  U_SFU_ROUT0.rout_up_cor_intf <> U_SFU_COR0.io.rout_cor_intf
  U_SFU_ROUT0.rout_dn_cor_intf <> U_SFU_COR1.io.rout_cor_intf
  U_SFU_ROUT0.rout_up_mid_intf <> U_SFU_MID0.io.rout_mid_intf
  U_SFU_ROUT0.rout_dn_mid_intf <> U_SFU_MID1.io.rout_mid_intf


  U_SFU_ROUT1.ib_swf_intf(0)   <> io.ib2_swf_intf
  U_SFU_ROUT1.ib_swf_intf(1)   <> io.ib3_swf_intf
  U_SFU_ROUT1.dsc_swf_intf     <> io.dsc_swf_intf(1)
  U_SFU_ROUT1.rout_up_cor_intf <> U_SFU_COR2.io.rout_cor_intf
  U_SFU_ROUT1.rout_dn_cor_intf <> U_SFU_COR3.io.rout_cor_intf
  U_SFU_ROUT1.rout_up_mid_intf <> U_SFU_MID2.io.rout_mid_intf
  U_SFU_ROUT1.rout_dn_mid_intf <> U_SFU_MID3.io.rout_mid_intf

  U_SFU_COR0.clk   := clk
  U_SFU_COR0.rst_n := rst_n
  U_SFU_COR1.clk   := clk
  U_SFU_COR1.rst_n := rst_n
  U_SFU_COR2.clk   := clk
  U_SFU_COR2.rst_n := rst_n
  U_SFU_COR3.clk   := clk
  U_SFU_COR3.rst_n := rst_n

  U_SFU_MID0.clk   := clk
  U_SFU_MID0.rst_n := rst_n
  U_SFU_MID1.clk   := clk
  U_SFU_MID1.rst_n := rst_n
  U_SFU_MID2.clk   := clk
  U_SFU_MID2.rst_n := rst_n
  U_SFU_MID3.clk   := clk
  U_SFU_MID3.rst_n := rst_n


  U_SFU_COR0.io.mid_cor_intf <> U_SFU_MID0.io.mid_cor_intf
  U_SFU_COR1.io.mid_cor_intf <> U_SFU_MID1.io.mid_cor_intf
  U_SFU_COR2.io.mid_cor_intf <> U_SFU_MID2.io.mid_cor_intf
  U_SFU_COR3.io.mid_cor_intf <> U_SFU_MID3.io.mid_cor_intf

  U_SFU_COR0.io.swf_obg0_intf <> io.swf_obg0_intf
  U_SFU_COR0.io.swf_obg1_intf <> io.swf_obg1_intf
  U_SFU_COR1.io.swf_obg0_intf <> io.swf_obg2_intf
  U_SFU_COR1.io.swf_obg1_intf <> io.swf_obg3_intf
  U_SFU_COR2.io.swf_obg0_intf <> io.swf_obg4_intf
  U_SFU_COR2.io.swf_obg1_intf <> io.swf_obg5_intf
  U_SFU_COR3.io.swf_obg0_intf <> io.swf_obg6_intf
  U_SFU_COR3.io.swf_obg1_intf <> io.swf_obg7_intf

  U_SFU_COR0.io.cfg <> io.cfg.cor(0)
  U_SFU_COR1.io.cfg <> io.cfg.cor(1)
  U_SFU_COR2.io.cfg <> io.cfg.cor(2)
  U_SFU_COR3.io.cfg <> io.cfg.cor(3)
  U_SFU_MID0.io.cfg <> io.cfg.mid(0)
  U_SFU_MID1.io.cfg <> io.cfg.mid(1)
  U_SFU_MID2.io.cfg <> io.cfg.mid(2)
  U_SFU_MID3.io.cfg <> io.cfg.mid(3)

  /**
   * Between MID and MID
   */
  U_SFU_MID0.io.mid_mid_intf.nbl_mid_data    := U_SFU_MID2.io.mid_mid_intf.mid_nbl_data
  U_SFU_MID0.io.mid_mid_intf.nbl_mid_data_bp := U_SFU_MID2.io.mid_mid_intf.mid_nbl_data_bp
  U_SFU_MID0.io.mid_mid_intf.nbl_mid_dsc     := U_SFU_MID2.io.mid_mid_intf.mid_nbl_dsc
  U_SFU_MID0.io.mid_mid_intf.nbl_mid_dsc_bp  := U_SFU_MID2.io.mid_mid_intf.mid_nbl_dsc_bp
  U_SFU_MID2.io.mid_mid_intf.nbl_mid_data    := U_SFU_MID0.io.mid_mid_intf.mid_nbl_data
  U_SFU_MID2.io.mid_mid_intf.nbl_mid_data_bp := U_SFU_MID0.io.mid_mid_intf.mid_nbl_data_bp
  U_SFU_MID2.io.mid_mid_intf.nbl_mid_dsc     := U_SFU_MID0.io.mid_mid_intf.mid_nbl_dsc
  U_SFU_MID2.io.mid_mid_intf.nbl_mid_dsc_bp  := U_SFU_MID0.io.mid_mid_intf.mid_nbl_dsc_bp

  U_SFU_MID1.io.mid_mid_intf.nbl_mid_data    := U_SFU_MID3.io.mid_mid_intf.mid_nbl_data
  U_SFU_MID1.io.mid_mid_intf.nbl_mid_data_bp := U_SFU_MID3.io.mid_mid_intf.mid_nbl_data_bp
  U_SFU_MID1.io.mid_mid_intf.nbl_mid_dsc     := U_SFU_MID3.io.mid_mid_intf.mid_nbl_dsc
  U_SFU_MID1.io.mid_mid_intf.nbl_mid_dsc_bp  := U_SFU_MID3.io.mid_mid_intf.mid_nbl_dsc_bp
  U_SFU_MID3.io.mid_mid_intf.nbl_mid_data    := U_SFU_MID1.io.mid_mid_intf.mid_nbl_data
  U_SFU_MID3.io.mid_mid_intf.nbl_mid_data_bp := U_SFU_MID1.io.mid_mid_intf.mid_nbl_data_bp
  U_SFU_MID3.io.mid_mid_intf.nbl_mid_dsc     := U_SFU_MID1.io.mid_mid_intf.mid_nbl_dsc
  U_SFU_MID3.io.mid_mid_intf.nbl_mid_dsc_bp  := U_SFU_MID1.io.mid_mid_intf.mid_nbl_dsc_bp

}



