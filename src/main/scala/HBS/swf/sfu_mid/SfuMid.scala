package HBS.swf.sfu_mid
import BaseCbb.data.GenModule
import BaseCbb.memory.fifo
import BaseCbb.misc._
import HBS.swf.common._
import HBS.swf.sfu_corner._
import chisel3._
import chisel3.util._
class SfuMid(p:SwfParams)  extends  GenModule{
  val clk                   = IO(Input(Clock()))
  val rst_n                 = IO(Input(Bool()))
  val io                    = IO(new Bundle{
    val rout_mid_intf       = new RoutSfuIntfBundle(p)
    val mid_cor_intf        = Flipped(new CorMidIntfBundle(p))
    val mid_mid_intf        = new MidMidIntfBundle(p)
    val cfg                 = Input(new MidSfuCfgBundle)
  })

  val data_fifo_rd  = Wire(Vec(2*p.SfuOutDp,Bool()))
  val nbl_mid_data  = DATA_FIFOs(data_fifo_rd)
  val dsc_fifo_rd   = Wire(Vec(2*p.SwfObgCp,Bool()))
  val nbl_mid_dsc   = DSC_FIFOs(dsc_fifo_rd)

  data_fifo_rd := nbl_mid_data._1
  dsc_fifo_rd  := nbl_mid_dsc._1
  val VerticalStageNum:Int        = math.ceil(48*2048*1.1/30000/0.7).toInt
  val DscVerticalBusNum: Int      = (p.IbSwfDsc + p.DscSwfDsc)/2
  val U_DATA_BUFFER = Module(new SfuBuffer(SrcNum = p.RoutSfuSwiDp, BufferDep = p.SfuDataBufDep, DataW = io.rout_mid_intf.data.head.req.data.bits.getWidth , QueueNum = p.NOBG*p.SfuOutDp, BpRtt =4,new InterfaceISAck(p)))
  U_DATA_BUFFER.clk                           := clk
  U_DATA_BUFFER.rst_n                         := rst_n
  U_DATA_BUFFER.io.i_enq_valid                := Seq2Vec(io.rout_mid_intf.data.map(x=>x.req.data.valid))
  U_DATA_BUFFER.io.i_enq_data                 := Seq2Vec(io.rout_mid_intf.data.map(x=>x.req.data.bits))
  U_DATA_BUFFER.io.i_enq_qid                  := Seq2Vec(io.rout_mid_intf.data.map(x=>x.req.data.bits.asTypeOf(new InterfaceISReqBundle(p)).target_lane))
  U_DATA_BUFFER.io.i_spr_bp                   := io.mid_cor_intf.cor_mid_data_spr_bp
  U_DATA_BUFFER.io.i_spr_pre_valid            := io.mid_cor_intf.cor_mid_data_spr_pre_valid
  U_DATA_BUFFER.io.cfg.wrr_weight             := io.cfg.hl_wrr_weight
  U_DATA_BUFFER.io.cfg.spr_cnt_th             := io.cfg.spr_cnt_th

  io.mid_cor_intf.mid_cor_data_spr_bp         := U_DATA_BUFFER.io.o_spr_bp
  io.mid_cor_intf.mid_cor_data_spr_pre_valid  := U_DATA_BUFFER.io.o_spr_pre_valid
  io.rout_mid_intf.data.zip(U_DATA_BUFFER.io.o_ack).foreach(x=>x._1.ack := x._2)

  val U_DATA_BUS_MATRIX = Module(new BusMatrix(io.rout_mid_intf.data.head.req.data.bits.cloneType,p.RoutSfuSwiDp,2*p.SfuOutDp))
  U_DATA_BUS_MATRIX.io.dn_in := U_DATA_BUFFER.io.o_deq_data
  U_DATA_BUS_MATRIX.io.lt_in.zip( io.mid_cor_intf.cor_mid_data).foreach(x=>x._1 := x._2.data)
  U_DATA_BUS_MATRIX.io.rt_in.zip(nbl_mid_data._2).foreach(x=>x._1.bits := x._2)
  U_DATA_BUS_MATRIX.io.rt_in.zip(data_fifo_rd).foreach(x=>x._1.valid:= x._2)
  io.mid_cor_intf.mid_cor_data.zip(U_DATA_BUS_MATRIX.io.lt_ot).foreach(x=>x._1.data := x._2)
  io.mid_mid_intf.mid_nbl_data.zip(U_DATA_BUS_MATRIX.io.rt_ot).foreach(x=>x._1.data := x._2)
  io.mid_mid_intf.mid_nbl_data_bp := nbl_mid_data._3


  private val U_DSC_BUFFER = Module(new SfuBuffer(SrcNum = p.RoutSfuSwiCtCp+p.RoutSfuSwiSafCp, BufferDep = p.SfuDataBufDep, DataW = io.rout_mid_intf.ct_dsc.head.req.data.bits.getWidth , QueueNum = p.NOBG*p.SwfObgCp, BpRtt =4,new InterfaceIDAck(p)))
  U_DSC_BUFFER.clk                    := clk
  U_DSC_BUFFER.rst_n                  := rst_n
  for(i<-0 until p.RoutSfuSwiCtCp){
    U_DSC_BUFFER.io.i_enq_valid(i)     := io.rout_mid_intf.ct_dsc(i).req.data.valid
    U_DSC_BUFFER.io.i_enq_data (i)     := io.rout_mid_intf.ct_dsc(i).req.data.bits
    U_DSC_BUFFER.io.i_enq_qid  (i)     := io.rout_mid_intf.ct_dsc(i).req.data.bits.asTypeOf(new InterfaceIDReqBundle(p)).target_lane
    io.rout_mid_intf.ct_dsc(i).ack     := U_DSC_BUFFER.io.o_ack(i)
  }
  for(i<-p.RoutSfuSwiCtCp until p.RoutSfuSwiCtCp+p.RoutSfuSwiSafCp){
    U_DSC_BUFFER.io.i_enq_valid(i)     := io.rout_mid_intf.saf_dsc(i-p.RoutSfuSwiCtCp).req.data.valid
    U_DSC_BUFFER.io.i_enq_data (i)     := io.rout_mid_intf.saf_dsc(i-p.RoutSfuSwiCtCp).req.data.bits
    U_DSC_BUFFER.io.i_enq_qid  (i)     := io.rout_mid_intf.saf_dsc(i-p.RoutSfuSwiCtCp).req.data.bits.asTypeOf(new InterfaceIDReqBundle(p)).target_lane
    io.rout_mid_intf.saf_dsc(i-p.RoutSfuSwiCtCp).ack := U_DSC_BUFFER.io.o_ack(i)
  }
  io.mid_cor_intf.mid_cor_dsc_spr_bp        := U_DSC_BUFFER.io.o_spr_bp
  U_DSC_BUFFER.io.i_spr_bp                  := io.mid_cor_intf.cor_mid_dsc_spr_bp
  U_DSC_BUFFER.io.i_spr_pre_valid           := io.mid_cor_intf.cor_mid_dsc_spr_pre_valid
  U_DSC_BUFFER.io.cfg.wrr_weight            := io.cfg.hl_wrr_weight
  U_DSC_BUFFER.io.cfg.spr_cnt_th            := io.cfg.spr_cnt_th
  io.mid_cor_intf.mid_cor_dsc_spr_pre_valid := U_DSC_BUFFER.io.o_spr_pre_valid

  val U_DSC_BUS_MATRIX = Module(new BusMatrix(io.rout_mid_intf.ct_dsc.head.req.data.bits.cloneType,p.RoutSfuSwiSafCp+p.RoutSfuSwiCtCp,2*p.SwfObgCp))
  U_DSC_BUS_MATRIX.io.dn_in            := U_DSC_BUFFER.io.o_deq_data
  U_DSC_BUS_MATRIX.io.lt_in.zip(io.mid_cor_intf.cor_mid_dsc).foreach(x=>x._1 := x._2.data)
  U_DSC_BUS_MATRIX.io.rt_in.zip(nbl_mid_dsc._2).foreach(x=>x._1.bits := x._2)
  U_DSC_BUS_MATRIX.io.rt_in.zip(dsc_fifo_rd).foreach(x=>x._1.valid := x._2)
  io.mid_cor_intf.mid_cor_dsc.zip(U_DSC_BUS_MATRIX.io.lt_ot).foreach(x=>x._1.data := x._2)
  io.mid_mid_intf.mid_nbl_dsc.zip(U_DSC_BUS_MATRIX.io.rt_ot).foreach(x=>x._1.data := x._2)
  io.mid_mid_intf.mid_nbl_dsc_bp := nbl_mid_dsc._3

  def DATA_FIFOs(fifo_rd:Vec[Bool]) = {
    val fifo_rdata = Wire(Vec(2*p.SfuOutDp,io.mid_mid_intf.nbl_mid_data.head.data.bits.cloneType))
    val fifo_ready = Wire(Vec(2*p.SfuOutDp,Bool()))
    val fifo_full  = Wire(Vec(2*p.SfuOutDp,Bool()))
    for(i<-0 until 2*p.SfuOutDp){
      val U_DATA_FIFO = Module(new fifo(io.mid_mid_intf.nbl_mid_data.head.data.bits.cloneType,16,0))
      U_DATA_FIFO.io.wp.we                := io.mid_mid_intf.nbl_mid_data(i).data.valid
      U_DATA_FIFO.io.wp.wdata             := io.mid_mid_intf.nbl_mid_data(i).data.bits
      io.mid_mid_intf.mid_nbl_data_bp(i)  := U_DATA_FIFO.io.wp.full
      U_DATA_FIFO.io.rp.re                := fifo_rd(i)
      fifo_rdata(i)                       := U_DATA_FIFO.io.rp.rdata
      fifo_ready(i)                       := !U_DATA_FIFO.io.rp.empty
      fifo_full(i)                        := U_DATA_FIFO.io.wp.full
    }
    (fifo_ready,fifo_rdata,fifo_full)
  }

  def DSC_FIFOs(fifo_rd:Vec[Bool]) = {
    val fifo_rdata = Wire(Vec(2*p.SwfObgCp,io.mid_mid_intf.nbl_mid_dsc.head.data.bits.cloneType))
    val fifo_ready = Wire(Vec(2*p.SwfObgCp,Bool()))
    val fifo_full  = Wire(Vec(2*p.SwfObgCp,Bool()))
    for(i<-0 until 2*p.SwfObgCp){
      val U_DSC_FIFO = Module(new fifo(io.mid_mid_intf.nbl_mid_dsc.head.data.bits.cloneType,16,0))
      U_DSC_FIFO.io.wp.we                := io.mid_mid_intf.nbl_mid_dsc(i).data.valid
      U_DSC_FIFO.io.wp.wdata             := io.mid_mid_intf.nbl_mid_dsc(i).data.bits
      io.mid_mid_intf.mid_nbl_data_bp(i) := U_DSC_FIFO.io.wp.full
      U_DSC_FIFO.io.rp.re                := fifo_rd(i)
      fifo_rdata(i)                      := U_DSC_FIFO.io.rp.rdata
      fifo_ready(i)                      := !U_DSC_FIFO.io.rp.empty
      fifo_full(i)                        := U_DSC_FIFO.io.wp.full
    }
    (fifo_ready,fifo_rdata,fifo_full)
  }
}
