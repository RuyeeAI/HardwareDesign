package BaseCbb.memory

import BaseCbb.GenModule
import chisel3._
import chisel3.util._
import firrtl.PrimOps.Pad
class SubLinklist(RamLat:Int,RscNum:Int,PtrW:Int) extends GenModule{
  val io = IO(new Bundle{
    val i_enq     = Input(Bool())
    val i_enq_ptr = Input(UInt(PtrW.W))
    val i_deq     = Input(Bool())
    val o_deq_ptr = Output(UInt(PtrW.W))
    val o_empty     = Output(Bool())
    val o_head_ptr  = Output(UInt(PtrW.W))
    val o_tail_ptr  = Output(UInt(PtrW.W))
    val ll_mem_intf = Flipped(new TpMemoryPort(log2Ceil(RscNum),PtrW))
  })
  val head_ptr           = RegInit(0.U(PtrW.W))
  val tail_ptr           = RegEnable(io.i_enq_ptr,io.i_enq)
  val link_mem_rdata_vld = ShiftRegister(io.ll_mem_intf.re,RamLat,false.B,true.B)
  val head_ptr_mux       = Mux(link_mem_rdata_vld,io.ll_mem_intf.rdata,head_ptr)
  val ll_empty           = RegInit(true.B)
  when((ll_empty && io.i_enq) || (head_ptr_mux===tail_ptr && io.i_deq && io.i_enq)){
    head_ptr := io.i_enq_ptr
  }.elsewhen(link_mem_rdata_vld){
    head_ptr := io.ll_mem_intf.rdata
  }

  when(ll_empty && io.i_enq){
    ll_empty := false.B
  }.elsewhen(!ll_empty && (head_ptr_mux=== tail_ptr) && io.i_deq && !io.i_enq){
    ll_empty := true.B
  }

  io.o_empty := ll_empty
  io.o_head_ptr := head_ptr
  io.o_tail_ptr := tail_ptr
  io.o_deq_ptr  := head_ptr_mux
  io.ll_mem_intf.waddr := tail_ptr
  io.ll_mem_intf.we    := io.i_enq && !ll_empty
  io.ll_mem_intf.wdata := io.i_enq_ptr
  io.ll_mem_intf.re    := io.i_deq && (head_ptr_mux =/= tail_ptr)
  io.ll_mem_intf.raddr := head_ptr_mux
}

class LinkList(RamLat:Int,RscNum:Int,PtrW:Int) extends GenModule {
  val io = IO(new Bundle{
    val i_enq       = Input(Bool())
    val i_enq_ptr   = Input(UInt(PtrW.W))
    val i_deq       = Input(Bool())
    val o_deq_ptr   = Output(UInt(PtrW.W))
    val o_empty     = Output(Bool())
    val o_full      = Output(Bool())
    val o_head_ptr  = Output(UInt(PtrW.W))
    val o_tail_ptr  = Output(UInt(PtrW.W))
    val ll_mem_intf = Flipped(new TpMemoryPort(log2Ceil(RscNum),PtrW))
  })
  def SubLlNum = RamLat
  val sub_deq_ptr   = Wire(Vec(SubLlNum,UInt(PtrW.W)))
  val sub_ll_empty  = Wire(Vec(SubLlNum,Bool()))
  val sub_head_ptr  = Wire(Vec(SubLlNum,UInt(PtrW.W)))
  val sub_tail_ptr  = Wire(Vec(SubLlNum,UInt(PtrW.W)))

  val sub_ram_re    = Wire(Vec(SubLlNum,Bool()))
  val sub_ram_we    = Wire(Vec(SubLlNum,Bool()))
  val sub_ram_waddr = Wire(Vec(SubLlNum,UInt(log2Ceil(RscNum).W)))
  val sub_ram_raddr = Wire(Vec(SubLlNum,UInt(log2Ceil(RscNum).W)))
  val sub_ram_wdata = Wire(Vec(SubLlNum,UInt(PtrW.W)))

  val (enq_seq,_) = Counter(io.i_enq,SubLlNum)
  val (deq_seq,_) = Counter(io.i_deq,SubLlNum)

  for(i<-0 until(SubLlNum))yield {
    val SubLl = Module(new SubLinklist(RamLat,RscNum,PtrW))
    SubLl.io.i_enq := io.i_enq && (enq_seq ===i.U)
    SubLl.io.i_enq_ptr := io.i_enq_ptr
    SubLl.io.i_deq := io.i_deq && (deq_seq === i.U)
    sub_deq_ptr(i) := SubLl.io.o_deq_ptr
    sub_ll_empty(i) := SubLl.io.o_empty
    sub_head_ptr(i) := SubLl.io.o_head_ptr
    sub_tail_ptr(i) := SubLl.io.o_tail_ptr
    sub_ram_re(i)   := SubLl.io.ll_mem_intf.re
    sub_ram_we(i)   := SubLl.io.ll_mem_intf.we
    sub_ram_waddr(i)   := SubLl.io.ll_mem_intf.waddr
    sub_ram_wdata(i)   := SubLl.io.ll_mem_intf.wdata
    sub_ram_raddr(i)   := SubLl.io.ll_mem_intf.raddr
    SubLl.io.ll_mem_intf.rdata := io.ll_mem_intf.rdata
  }

  io.ll_mem_intf.we := sub_ram_we.reduceTree(_|_)
  io.ll_mem_intf.re := sub_ram_re.reduceTree(_|_)
  io.ll_mem_intf.waddr := sub_ram_waddr.zip(sub_ram_we).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_)
  io.ll_mem_intf.wdata := sub_ram_wdata.zip(sub_ram_we).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_)
  io.ll_mem_intf.raddr := sub_ram_raddr.zip(sub_ram_re).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_)

  val ll_cnt = RegInit(0.U(log2Ceil(RscNum+1).W))
  val ll_full = RegInit(false.B)
  when(io.i_deq && !io.i_enq){
    ll_cnt := ll_cnt -1.U
  }.elsewhen(!io.i_deq && io.i_enq){
    ll_cnt := ll_cnt + 1.U
  }

  when(io.i_enq && ll_cnt ===(RscNum-1).U && !io.i_deq){
    ll_full := true.B
  }.elsewhen(ll_cnt === RscNum.U && !io.i_enq && io.i_deq){
    ll_full := false.B
  }

  io.o_empty := sub_ll_empty.reduceTree(_&&_)
  io.o_deq_ptr := sub_deq_ptr(deq_seq)
  io.o_full := ll_full
  io.o_tail_ptr := sub_tail_ptr(enq_seq)
  io.o_head_ptr := sub_head_ptr(deq_seq)
}

class VoqLinkList(QueueNum:Int,RamLat:Int,RscNum:Int,PtrW:Int) extends GenModule{
  val io = IO(new Bundle {
    val i_enq     = Input(Bool())
    val i_enq_ptr = Input(UInt(PtrW.W))
    val i_enq_qid = Input(UInt(log2Ceil(QueueNum).W))
    val i_deq     = Input(Vec(QueueNum, Bool()))
    val o_deq_ptr = Output(UInt(PtrW.W))
    val o_empty   = Output(Bool())
    val o_full    = Output(Bool())
    val o_head_ptr = Output(UInt(PtrW.W))
    val o_tail_ptr = Output(UInt(PtrW.W))
    val ll_mem_intf = Flipped(new TpMemoryPort(log2Ceil(RscNum), PtrW))
  })

  /**
   * It seems this architecture consume a lot of headers & tails:
   * each OBG need 12 lanes, 4 OBG per 57.6T die.
   * per destination queue, it need 4 headers & 4 tails, assume each header and tail us 6 bits
   * total need: 6b*8(H&T)*48(dest num per control point) = 2304b
   * it might need prefetch logic later to resolve the timing issue.
   */
  def SubLlNum      = RamLat*QueueNum
  val sub_deq_ptr   = Wire(Vec(SubLlNum,UInt(PtrW.W)))
  val sub_ll_empty  = Wire(Vec(SubLlNum,Bool()))
  val sub_head_ptr  = Wire(Vec(SubLlNum,UInt(PtrW.W)))
  val sub_tail_ptr  = Wire(Vec(SubLlNum,UInt(PtrW.W)))

  val sub_ram_re    = Wire(Vec(SubLlNum,Bool()))
  val sub_ram_we    = Wire(Vec(SubLlNum,Bool()))
  val sub_ram_waddr = Wire(Vec(SubLlNum,UInt(log2Ceil(RscNum).W)))
  val sub_ram_raddr = Wire(Vec(SubLlNum,UInt(log2Ceil(RscNum).W)))
  val sub_ram_wdata = Wire(Vec(SubLlNum,UInt(PtrW.W)))
  val deq           = io.i_deq.reduceTree(_|_)

  val enq_seq = Wire(Vec(QueueNum,UInt(log2Ceil(RamLat).W)))
  val deq_seq = Wire(Vec(QueueNum,UInt(log2Ceil(RamLat).W)))

  for(q<-0 until QueueNum){
    enq_seq(q) := Counter(io.i_enq && io.i_enq_qid === q.U,QueueNum)._1
    deq_seq(q) := Counter(io.i_deq(q),QueueNum)._1
  }

  for(i<-0 until(SubLlNum))yield {
    val SubLl = Module(new SubLinklist(RamLat,RscNum,PtrW))
    SubLl.io.i_enq      := io.i_enq && (enq_seq(io.i_enq_qid) ===(i%RamLat).U)
    SubLl.io.i_enq_ptr  := io.i_enq_ptr
    SubLl.io.i_deq      := io.i_deq(math.floor(i/QueueNum).toInt) && (deq_seq(math.floor(i/QueueNum).toInt)=== (i%RamLat).U)
    sub_deq_ptr(i)  := SubLl.io.o_deq_ptr
    sub_ll_empty(i) := SubLl.io.o_empty
    sub_head_ptr(i) := SubLl.io.o_head_ptr
    sub_tail_ptr(i) := SubLl.io.o_tail_ptr
    sub_ram_re(i)   := SubLl.io.ll_mem_intf.re
    sub_ram_we(i)   := SubLl.io.ll_mem_intf.we
    sub_ram_waddr(i)   := SubLl.io.ll_mem_intf.waddr
    sub_ram_wdata(i)   := SubLl.io.ll_mem_intf.wdata
    sub_ram_raddr(i)   := SubLl.io.ll_mem_intf.raddr
    SubLl.io.ll_mem_intf.rdata := io.ll_mem_intf.rdata
  }
  io.ll_mem_intf.we    := sub_ram_we.reduceTree(_|_)
  io.ll_mem_intf.re    := sub_ram_re.reduceTree(_|_)
  io.ll_mem_intf.waddr := sub_ram_waddr.zip(sub_ram_we).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_)
  io.ll_mem_intf.wdata := sub_ram_wdata.zip(sub_ram_we).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_)
  io.ll_mem_intf.raddr := sub_ram_raddr.zip(sub_ram_re).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_)

  val ll_cnt = RegInit(0.U(log2Ceil(RscNum+1).W))
  val ll_full = RegInit(false.B)

  when(io.i_deq.reduceTree(_|_) && !io.i_enq){
    ll_cnt := ll_cnt -1.U
  }.elsewhen(!io.i_deq.reduceTree(_|_) && io.i_enq){
    ll_cnt := ll_cnt + 1.U
  }

  when(io.i_enq && ll_cnt ===(RscNum-1).U && !deq){
    ll_full := true.B
  }.elsewhen(ll_cnt === RscNum.U && !io.i_enq && deq){
    ll_full := false.B
  }
  val deq_sel = deqSel(io.i_deq,deq_seq.zip(io.i_deq).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_))
  io.o_empty    := sub_ll_empty.reduceTree(_&&_)
  io.o_deq_ptr  := sub_deq_ptr.zip(deq_sel).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_)
  io.o_full     := ll_full
  val enq_sel   = enqSel(io.i_enq_qid,enq_seq(io.i_enq_qid))
  io.o_tail_ptr := sub_tail_ptr.zip(enq_sel).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_)
  io.o_head_ptr := sub_head_ptr.zip(deq_sel).map(x=>Mux(x._2,x._1,0.U)).reduce(_|_)


  def deqSel(deq_vld:Vec[Bool],seq:UInt)={
    val w = Wire(Vec(QueueNum*RamLat,Bool()))
    for(i<-0 until QueueNum*RamLat){
      w(i) := deq_vld(math.floor(i/QueueNum).toInt) && (i%QueueNum).U === seq
    }
    w
  }
  def enqSel(qid:UInt,seq:UInt)={
    val w = Wire(Vec(QueueNum*RamLat,Bool()))
    for(i<-0 until QueueNum*RamLat){
      w(i) := qid===math.ceil(i/RamLat).toInt.U && (i%QueueNum).U === seq
    }
    w
  }

}