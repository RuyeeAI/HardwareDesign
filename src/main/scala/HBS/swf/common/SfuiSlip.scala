package HBS.swf.common

import BaseCbb.data.GenModule
// RrLogic / WRR 由 BaseCbb/arbiter/arbiter.scala 声明在裸包 BaseCbb 下（HD 既有结构）
import BaseCbb.{RrLogic, WRR}
import BaseCbb.arbiter.iSlipLogic
import BaseCbb.misc.{Convert2dArray, Seq2Vec}
import chisel3._
import chisel3.util._

class SfuiSlip(SrcNum:Int,
                           DstNum:Int,
                           Loc:Int,
                           IterNum:Int=2,
                           ) extends GenModule{
  val io = IO(new Bundle{
    val i_req             = Input(Vec(SrcNum,Vec(DstNum,Bool())))
    val spr_pre_valid     = Input(Vec(DstNum,Bool()))
    val spr_bp_dly        = Input(Vec(DstNum,Bool()))
    val cfg               = Input(new SfuDataCfgBundle)
    val o_gnt             = Output(Vec(SrcNum,Vec(DstNum,Bool())))
  })

  val rc                 = io.i_req
  val src_req_mask       = RegInit(VecInit(Seq.fill(SrcNum)(true.B)))
  val dst_req_mask       = RegInit(VecInit(Seq.fill(DstNum)(true.B)))
  val islip_lgc_req      = Wire(Vec(SrcNum,Vec(DstNum,Bool())))
  val islip_lgc_gnt      = Wire(Vec(SrcNum,Vec(DstNum,Bool()))) // The Grant generate per cycle
  val islip_lgc_gnt_conv = Convert2dArray(islip_lgc_gnt)
  val tdm                = RegInit(Loc.U(log2Ceil(IterNum).W))
  val islip_src_gnt      = islip_lgc_gnt.map(x=>x.reduceTree(_|_))
  val islip_dst_gnt      = islip_lgc_gnt_conv.map(x=>x.reduceTree(_|_))

  /**
   * TDM
   * {{{
   *   * TDM == 0
   *     scheduler work and
   * }}}
   */
  tdm := tdm + 1.U
  val last_tdm_cycle = tdm === (IterNum-1).U

  //Generate request
  for(s<- 0 until SrcNum){
    for(d<-0 until DstNum){

    }
  }

  /**
   * Generate the Request to the ISLIP, the src_req_mask are all valid when TDM == 0.
   * each cycle when there is grant to one source port, mask all the request from the source
   */

  when(last_tdm_cycle){
    src_req_mask := Fill(SrcNum,false.B).asTypeOf(src_req_mask.cloneType)
  }.otherwise{
    src_req_mask := src_req_mask.zip(islip_src_gnt).map(x=>x._1 || x._2)
  }

  when(last_tdm_cycle){
    dst_req_mask := Fill(DstNum,false.B).asTypeOf(dst_req_mask.cloneType)
  }.otherwise{
    dst_req_mask := dst_req_mask.zip(islip_dst_gnt).map(x=>x._1  || x._2)
  }

  for(s<-0 until SrcNum){
    for(d<-0 until DstNum){
      islip_lgc_req(s)(d) := Mux(src_req_mask(s)^dst_req_mask(d),false.B,rc(s)(d))
    }
  }

  //The request RR per destination
  val req_ptr    = RegInit(0.U.asTypeOf(Vec(DstNum,Vec(SrcNum,Bool()))))
  val b_req      = Wire(Vec(DstNum,Vec(SrcNum,Bool())))
  val b_gnt      = Wire(Vec(DstNum,Vec(SrcNum,Bool())))
  val acc_ptr_h  = RegInit(0.U.asTypeOf(Vec(SrcNum,Vec(DstNum,Bool()))))
  val acc_ptr_l  = RegInit(0.U.asTypeOf(Vec(SrcNum,Vec(DstNum,Bool()))))

  val a_req     = Wire(Vec(SrcNum,Vec(DstNum,Bool())))
  val a_req_h   = Wire(Vec(SrcNum,Vec(DstNum,Bool())))
  val a_req_l   = Wire(Vec(SrcNum,Vec(DstNum,Bool())))
  //val a_gnt     = Wire(Vec(SrcNum,Vec(DstNum,Bool())))

  for (d <- 0 until (DstNum)) {
    for (s <- 0 until SrcNum) {
      b_req(d)(s) := islip_lgc_req(s)(d)
    }
    b_gnt(d) := RrLogic(b_req(d).asUInt, req_ptr(d).asUInt).asTypeOf(b_gnt.head.cloneType)
  }

  /**
   * {{{
   *  Maintain the pointer of the scheduler
   *   req_ptr is updated when:
   *  * tdm is IterNum-1
   *  * Destination is granted.
   *  * spr_pre_valid is not set to source grant destination ports
   *
   * }}}
   */

  for(d<-0 until DstNum){
    val req_ptr_update = last_tdm_cycle && islip_lgc_gnt_conv(d).reduceTree(_|_) && !io.spr_pre_valid(d)
    when(req_ptr_update){
      req_ptr(d) := islip_lgc_gnt_conv(d).asTypeOf(req_ptr.head.cloneType)
    }
  }

  /**
   * when wrr select high priority request to accept request is
   */
  for(s<-0 until SrcNum){
    val wrr_gnt    = Wire(Vec(2,Bool()))
    val WRR        = Module(new WRR(ClientNum = 2,WtWidth = 5))
    WRR.io.ready  := Cat(a_req_h(s).reduceTree(_|_),a_req_l(s).reduceTree(_|_))
    WRR.io.enable := a_req(s).reduceTree(_|_)
    WRR.io.weight := io.cfg.wrr_weight
    wrr_gnt       := WRR.io.grant.asTypeOf(wrr_gnt.cloneType)
    val acc_ptr    = Mux(wrr_gnt(0), acc_ptr_h(s), acc_ptr_l(s))

    for(d<-0 until DstNum){
      a_req_h(s)(d) := Mux(io.spr_bp_dly(d),b_gnt(d)(s),false.B)
      a_req_l(s)(d) := Mux(!io.spr_bp_dly(d),b_gnt(d)(s),false.B)
      a_req(s)(d) := Mux(wrr_gnt(0),a_req_h(s)(d),a_req_l(s)(d))
    }
    islip_lgc_gnt(s) := RrLogic(a_req(s).asUInt, acc_ptr.asUInt).asTypeOf(islip_lgc_gnt.head.cloneType)
    /**
     * {{{
     *  accept ptr is updated when:
     *  * tdm is IterNum -1
     *  * spr_pre_valid is not set
     *  * destination is granted
     * }}}
     */
    val acc_ptr_update = last_tdm_cycle && islip_lgc_gnt(s).reduceTree(_|_) && (islip_lgc_gnt(s).zip(io.spr_pre_valid).map(x=>x._1 && !x._2).reduce(_|_))
    when(acc_ptr_update && wrr_gnt(0)){
      acc_ptr_h(s) := islip_lgc_gnt(s)
    }.elsewhen(acc_ptr_update && !wrr_gnt(0)){
      acc_ptr_l(s) := islip_lgc_gnt(s)
    }
  }

  io.o_gnt := islip_lgc_gnt
}






















