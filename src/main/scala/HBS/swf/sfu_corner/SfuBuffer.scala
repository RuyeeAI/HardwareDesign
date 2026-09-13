package HBS.swf.sfu_corner

import BaseCbb.data.{GenBundle, GenModule}
import BaseCbb.misc.Seq2Vec
import HBS.swf.common._
import chisel3._
import chisel3.util._



class SfuBuffer[T<:Data](SrcNum:Int,BufferDep:Int,DataW:Int,QueueNum:Int,BpRtt:Int,at:T) extends GenModule{
  val clk         = IO(Input(Clock()))
  val rst_n       = IO(Input(Bool()))
  val io = IO(new Bundle{
    val i_enq_data      = Input(Vec(SrcNum,UInt(DataW.W)))
    val i_enq_valid     = Input(Vec(SrcNum, Bool()))
    val i_enq_qid       = Input(Vec(SrcNum,UInt(log2Ceil(QueueNum).W)))
    val o_ack           = Output(Vec(SrcNum,Valid(at)))
    val cfg             = Input(new SfuDataCfgBundle)
    val i_spr_bp        = Input(Vec(QueueNum/2,Bool()))
    val i_spr_pre_valid = Input(Vec(QueueNum/2,Bool()))
    val o_spr_bp        = Output(Vec(QueueNum/2,Bool()))
    val o_spr_pre_valid = Output(Vec(QueueNum/2,Bool()))
    val o_deq_data      = Output(Vec(SrcNum,Valid(new BufferOutBundle(DataW = DataW, DstW = log2Ceil(QueueNum)))))
  })

  val deq_valid = Wire(Vec(SrcNum,Vec(QueueNum,Bool())))
  val deq_data  = Wire(Vec(SrcNum,Valid(UInt(DataW.W))))
  val enq_valid = Wire(Vec(SrcNum,new VoqEnqReq(DstNum = QueueNum)))
  for(i<-0 until SrcNum){
    val U_DATA_BUFFER = Module(new VoqBuffer(BufferDep = BufferDep, DataW = DataW, QueueNum = QueueNum))
    U_DATA_BUFFER.clk := clk
    U_DATA_BUFFER.rst_n := rst_n
    U_DATA_BUFFER.io.i_enq_valid := io.i_enq_valid(i)
    U_DATA_BUFFER.io.i_enq_qid   := io.i_enq_qid(i)
    U_DATA_BUFFER.io.i_enq_data  := io.i_enq_data(i)
    U_DATA_BUFFER.io.i_deq_valid := deq_valid(i)
    deq_data(i)                  := U_DATA_BUFFER.io.o_deq_data

    enq_valid(i).vld             := RegNext(io.i_enq_valid(i),false.B)
    enq_valid(i).dst             := RegEnable(io.i_enq_qid(i),io.i_enq_valid(i))

    io.o_deq_data(i).bits.data   := U_DATA_BUFFER.io.o_deq_data.bits
    io.o_deq_data(i).bits.dst    := PriorityEncoder(deq_valid(i))
    io.o_deq_data(i).valid       := deq_valid(i).reduceTree(_|_)

    io.o_ack(i).valid := deq_valid(i).reduceTree(_|_)
    io.o_ack(i).bits  := 0.U.asTypeOf(at) //TODO
  }

  val U_DATA_CONTROL = Module(new SfuController(SrcNum=SrcNum , DstNum =QueueNum , IterNum = 2, SfuMemDep = BufferDep, BpRtt = BpRtt))
  U_DATA_CONTROL.io.voq_enq       := enq_valid
  U_DATA_CONTROL.io.spr_pre_valid := io.i_spr_pre_valid
  io.o_spr_bp                     := U_DATA_CONTROL.io.o_spr_bp
  deq_valid                       := U_DATA_CONTROL.io.o_sch_gnt
  U_DATA_CONTROL.io.i_spr_bp      := io.i_spr_bp
  U_DATA_CONTROL.io.cfg           := io.cfg

  io.o_spr_pre_valid := 0.U.asTypeOf(io.o_spr_pre_valid.cloneType) //TODO
}
