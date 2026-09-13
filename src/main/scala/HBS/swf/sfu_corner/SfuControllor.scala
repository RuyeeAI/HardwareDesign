package HBS.swf.sfu_corner

import BaseCbb.misc.{Convert2dArray, LatencyPipe}
import BaseCbb.data.{GenBundle, GenModule}
import HBS.swf.common._
import chisel3._
import chisel3.util.{Fill, Pipe, log2Ceil}

class SfuController(
                     SrcNum:Int,
                     DstNum:Int,
                     IterNum:Int=2,
                     SfuMemDep:Int=32,
                     BpRtt:Int=4
                   ) extends GenModule{
  val io = IO(new Bundle{
    val voq_enq           = Input(Vec(SrcNum,new VoqEnqReq(DstNum)))
    val cfg               = Input(new SfuDataCfgBundle)
    val spr_pre_valid     = Input(Vec(DstNum/2,Bool()))
    val o_spr_bp          = Output(Vec(DstNum/2,Bool()))
    val i_spr_bp          = Input(Vec(DstNum/2,Bool()))
    val o_sch_gnt         = Output(Vec(SrcNum,Vec(DstNum,Bool())))
  })
  val sch_gnt           = Wire(Vec(SrcNum,Vec(DstNum,Bool())))
  val sub_sch_gnt       = Wire(Vec(IterNum, Vec(SrcNum,Vec(DstNum,Bool()))))
  val sch_gnt_dst       = Convert2dArray(sch_gnt).map(x=>x.reduceTree(_|_))
  val voq_req_cnt       = RegInit(0.U.asTypeOf(Vec(SrcNum,Vec(DstNum,UInt(log2Ceil(SfuMemDep).W)))))
  val sub_islip_req     = RegInit(0.U.asTypeOf(Vec(IterNum,Vec(SrcNum,Vec(DstNum,Bool())))))
  val voq_inc           = Wire(Vec(SrcNum,Vec(DstNum,Bool())))
  val voq_dec           = Wire(Vec(SrcNum,Vec(DstNum,Bool())))

  val tdm = RegInit(0.U(log2Ceil(IterNum).W))
  tdm     := Mux(tdm === (IterNum-1).U, 0.U,tdm+1.U)


  val spr_bp       = GenSprBp
  val spr_bp_ff    = Pipe(true.B,spr_bp,BpRtt).bits
  io.o_spr_bp     := spr_bp
  val sub_sch_req  = VoqRequest

  // Sub Scheduler
  for(i<-0 until IterNum) {
    val U_SUB_SCH = Module(new SfuiSlip(SrcNum = SrcNum, DstNum = DstNum, Loc = i, IterNum = IterNum))
    U_SUB_SCH.io.cfg           := io.cfg
    U_SUB_SCH.io.i_req         := sub_sch_req(i)
    U_SUB_SCH.io.spr_pre_valid := 0.U.asTypeOf(Vec(DstNum,Bool())) //Vec(2,io.spr_pre_valid).asTypeOf(U_SUB_SCH.io.spr_pre_valid.cloneType) //TODO
    U_SUB_SCH.io.spr_bp_dly    := 0.U.asTypeOf(Vec(DstNum,Bool()))//spr_bp_ff
    sub_sch_gnt(i)             := U_SUB_SCH.io.o_gnt
  }
  sch_gnt := sub_sch_gnt(tdm)
  io.o_sch_gnt := sch_gnt
  def VoqRequest = {
    for(s<-0 until SrcNum){
      for(d<-0 until DstNum){
        voq_inc(s)(d)            := io.voq_enq(s).vld && io.voq_enq(s).dst===d.U
        voq_dec(s)(d)            := (!sub_islip_req((IterNum-1).U-tdm)(s)(d) || sch_gnt(s)(d)) && voq_req_cnt(s)(d)>0.U
        voq_req_cnt(s)(d)        := voq_req_cnt(s)(d) + voq_inc(s)(d) - voq_dec(s)(d)
        for(i<-0 until IterNum){
          when(tdm===(IterNum-1).U){
            when(voq_req_cnt(s)(d)>0.U){
              sub_islip_req(i)(s)(d) := true.B
            }.elsewhen(sub_sch_gnt(i)(s)(d) && voq_req_cnt(s)(d)===0.U){
              sub_islip_req(i)(s)(d) := false.B
            }
          }
        }
      }
    }
    sub_islip_req
  }




    // Generate SPR BP according to spr_cnt and queue fill lvl
  def GenSprBp:Vec[Bool]={
    /**
     * Generate SPR_BP
     */
    val spr_bp_cnt       = RegInit(VecInit(Seq.fill(DstNum/2)(0.U(4.W))))
    val spr_bp           = RegInit(VecInit(Seq.fill(DstNum/2)(false.B)))
    val spr_bp_last_cnt  = RegInit(VecInit(Seq.fill(DstNum/2)(0.U(4.W))))
    val queue_dst_cnt    = RegInit(VecInit(Seq.fill(DstNum/2)(0.U(log2Ceil(SfuMemDep).W))))

    for(d<-0 until DstNum/2){
      val spr_bp_cnt_reach_th = spr_bp_cnt(d)=== io.cfg.spr_cnt_th
      when(io.spr_pre_valid(d)){
        spr_bp_cnt(d) := spr_bp_cnt(d) + 1.U
      }.elsewhen(spr_bp_cnt_reach_th){
        spr_bp_cnt(d) := 0.U
      }

      when(spr_bp_cnt_reach_th){
        spr_bp(d) := true.B
      }.elsewhen(spr_bp_last_cnt(d)===0.U && spr_bp(d)){
        spr_bp(d) := false.B
      }

      when(spr_bp_cnt_reach_th){
        spr_bp_last_cnt(d) := queue_dst_cnt(d)-BpRtt.U
      }.elsewhen(spr_bp_last_cnt(d)>0.U){
        when(sch_gnt_dst(d)){
          spr_bp_last_cnt(d) := spr_bp_last_cnt(d) -1.U
        }
      }

      /**
       * Count the queue fill level to target queue
       */
      queue_dst_cnt(d):= queue_dst_cnt(d) + io.voq_enq.map(x=>Mux(x.vld && x.dst===d.U,1.U,0.U)).reduce(_+_) - sch_gnt_dst(d)
    }
    spr_bp
  }






}

