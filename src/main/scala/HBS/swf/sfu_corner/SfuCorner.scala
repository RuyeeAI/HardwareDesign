package HBS.swf.sfu_corner

import BaseCbb.data.GenModule
import BaseCbb.misc.Seq2Vec
import HBS.swf.common._
import chisel3._
import chisel3.util._


class SfuCorner (p:SwfParams) extends GenModule{
  val clk   = IO(Input(Clock()))
  val rst_n = IO(Input(Bool()))
  val io = IO(new Bundle{
    // from Routing to Corner
    val rout_cor_intf        = new RoutSfuIntfBundle(p)
    // from Corner to OBG
    val swf_obg0_intf        = new SwfObgIntf(p)
    val swf_obg1_intf        = new SwfObgIntf(p)
    // from Middle to Corner
    val mid_cor_intf         = new CorMidIntfBundle(p)
    val cfg                  = Input(new SfuDataCfgBundle)
  })

  //rout_cor_intf.data.
  val DscVerticalBusNum: Int      = (p.IbSwfDsc + p.DscSwfDsc)/2

  // Data & Data Controls

  val U_DATA_BUFFER = Module(new SfuBuffer(SrcNum = p.RoutSfuSwiDp, BufferDep = p.SfuDataBufDep, DataW = io.rout_cor_intf.data.head.req.data.bits.getWidth , QueueNum = p.NOBG*p.SfuOutDp, BpRtt =4,new InterfaceISAck(p)))
  U_DATA_BUFFER.clk                           := clk
  U_DATA_BUFFER.rst_n                         := rst_n
  U_DATA_BUFFER.io.i_enq_valid                := Seq2Vec(io.rout_cor_intf.data.map(x=>x.req.data.valid))
  U_DATA_BUFFER.io.i_enq_data                 := Seq2Vec(io.rout_cor_intf.data.map(x=>x.req.data.bits))
  U_DATA_BUFFER.io.i_enq_qid                  := Seq2Vec(io.rout_cor_intf.data.map(x=>x.req.data.bits.asTypeOf(new InterfaceISReqBundle(p)).target_lane))
  io.mid_cor_intf.cor_mid_data_spr_bp         := U_DATA_BUFFER.io.o_spr_bp
  io.mid_cor_intf.cor_mid_data_spr_pre_valid  := U_DATA_BUFFER.io.o_spr_pre_valid
  U_DATA_BUFFER.io.i_spr_bp                   := io.mid_cor_intf.mid_cor_data_spr_bp
  U_DATA_BUFFER.io.i_spr_pre_valid            := io.mid_cor_intf.mid_cor_data_spr_pre_valid
  U_DATA_BUFFER.io.cfg              := io.cfg
  io.rout_cor_intf.data.zip(U_DATA_BUFFER.io.o_ack).foreach(x=>x._1.ack := x._2)

  val U_DATA_BUS_MATRIX = Module(new BusMatrix(io.rout_cor_intf.data.head.req.data.bits.cloneType,p.RoutSfuSwiDp,2*p.SfuOutDp))
  U_DATA_BUS_MATRIX.io.dn_in := U_DATA_BUFFER.io.o_deq_data
  U_DATA_BUS_MATRIX.io.lt_in.foreach(x=>x.valid := false.B)
  U_DATA_BUS_MATRIX.io.lt_in.foreach(x=>x.bits  := 0.U.asTypeOf(x.bits.cloneType))
  U_DATA_BUS_MATRIX.io.rt_in.zip(io.mid_cor_intf.mid_cor_data).foreach(x=>x._1      := x._2.data)
  U_DATA_BUS_MATRIX.io.rt_ot.zip(io.mid_cor_intf.cor_mid_data).foreach(x=>x._2.data := x._1)

  for(i<-0 until(p.SfuOutDp)){
    io.swf_obg0_intf.data(i).data := U_DATA_BUS_MATRIX.io.lt_ot(i)
    io.swf_obg1_intf.data(i).data := U_DATA_BUS_MATRIX.io.lt_ot(i+p.SfuOutDp)
  }


  private val U_DSC_BUFFER = Module(new SfuBuffer(SrcNum = p.RoutSfuSwiCtCp+p.RoutSfuSwiSafCp, BufferDep = p.SfuDataBufDep, DataW = io.rout_cor_intf.ct_dsc.head.req.data.bits.getWidth , QueueNum = p.NOBG*p.SwfObgCp, BpRtt =4,new InterfaceIDAck(p)))
  U_DSC_BUFFER.clk                := clk
  U_DSC_BUFFER.rst_n              := rst_n
  for(i<-0 until p.RoutSfuSwiCtCp){
    U_DSC_BUFFER.io.i_enq_valid(i)     := io.rout_cor_intf.ct_dsc(i).req.data.valid
    U_DSC_BUFFER.io.i_enq_data (i)     := io.rout_cor_intf.ct_dsc(i).req.data.bits
    U_DSC_BUFFER.io.i_enq_qid  (i)     := io.rout_cor_intf.ct_dsc(i).req.data.bits.asTypeOf(new InterfaceIDReqBundle(p)).target_lane
    io.rout_cor_intf.ct_dsc(i).ack     := U_DSC_BUFFER.io.o_ack(i)
  }
  for(i<-p.RoutSfuSwiCtCp until p.RoutSfuSwiCtCp+p.RoutSfuSwiSafCp){
    U_DSC_BUFFER.io.i_enq_valid(i)     := io.rout_cor_intf.saf_dsc(i-p.RoutSfuSwiCtCp).req.data.valid
    U_DSC_BUFFER.io.i_enq_data (i)     := io.rout_cor_intf.saf_dsc(i-p.RoutSfuSwiCtCp).req.data.bits
    U_DSC_BUFFER.io.i_enq_qid  (i)     := io.rout_cor_intf.saf_dsc(i-p.RoutSfuSwiCtCp).req.data.bits.asTypeOf(new InterfaceIDReqBundle(p)).target_lane
    io.rout_cor_intf.saf_dsc(i-p.RoutSfuSwiCtCp).ack := U_DSC_BUFFER.io.o_ack(i)
  }

  io.mid_cor_intf.cor_mid_dsc_spr_bp    := U_DSC_BUFFER.io.o_spr_bp
  U_DSC_BUFFER.io.i_spr_bp              := io.mid_cor_intf.mid_cor_dsc_spr_bp
  U_DSC_BUFFER.io.i_spr_pre_valid       := io.mid_cor_intf.mid_cor_dsc_spr_pre_valid
  U_DSC_BUFFER.io.cfg                   := io.cfg

  val U_DSC_BUS_MATRIX = Module(new BusMatrix(io.rout_cor_intf.ct_dsc.head.req.data.bits.cloneType,SrcNum = p.RoutSfuSwiCtCp+p.RoutSfuSwiSafCp, 2*p.SwfObgCp))
  U_DSC_BUS_MATRIX.io.dn_in := U_DSC_BUFFER.io.o_deq_data
  U_DSC_BUS_MATRIX.io.lt_in.foreach(x=>x.valid := false.B)
  U_DSC_BUS_MATRIX.io.lt_in.foreach(x=>x.bits  := 0.U.asTypeOf(x.bits.cloneType))
  U_DSC_BUS_MATRIX.io.rt_in.zip(io.mid_cor_intf.mid_cor_dsc).foreach(x=>x._1      := x._2.data)
  U_DSC_BUS_MATRIX.io.rt_ot.zip(io.mid_cor_intf.cor_mid_dsc).foreach(x=>x._2.data := x._1)

  for(i<-0 until(p.SwfObgCp)){
    io.swf_obg0_intf.dsc(i).data := U_DSC_BUS_MATRIX.io.lt_ot(i)
    io.swf_obg1_intf.dsc(i).data := U_DSC_BUS_MATRIX.io.lt_ot(i+p.SwfObgCp)
  }
}

