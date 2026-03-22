package BaseCbb.arbiter

import BaseCbb._
import BaseCbb.utils.Seq2Vec
import chisel3._


class iSlipLogic(SrcNum:Int,DstNum:Int) extends GenModule{
  val io = IO(new Bundle{
    val req = Input(Vec(SrcNum,Vec(DstNum,Bool())))

    val src_ptr = Input(Vec(SrcNum,UInt(DstNum.W)))
    val dst_ptr = Input(Vec(DstNum,UInt(SrcNum.W)))
    val gnt     = Output(Vec(SrcNum,Vec(DstNum,Bool())))
    val b_gnt   = Output(Vec(DstNum,Vec(SrcNum,Bool())))
  })

  val b_req   = Wire(Vec(DstNum,Vec(SrcNum,Bool())))
  val b_gnt   = Wire(Vec(DstNum,Vec(SrcNum,Bool())))

  val a_req   = Wire(Vec(SrcNum,Vec(DstNum,Bool())))
  val a_gnt   = Wire(Vec(SrcNum,Vec(DstNum,Bool())))

    for (d <- 0 until (DstNum)) {
      for (s <- 0 until SrcNum) {
        b_req(d)(s) := io.req(s)(d)
      }
      b_gnt(d) := RrLogic(b_req(d).asUInt, io.dst_ptr(d)).asTypeOf(b_gnt(d))
    }

    for (s <- 0 until SrcNum) {
      for (d <- 0 until DstNum) {
        a_req(s)(d) := b_gnt(d)(s)
      }
      a_gnt(s) := RrLogic(a_req(s).asUInt, io.src_ptr(s)).asTypeOf(a_gnt(s))
    }

  io.gnt := a_gnt.asTypeOf(io.gnt)
  io.b_gnt := b_gnt
}









class RegulariSlip (SrcNum:Int,DstNum:Int) extends GenModule {
  val io = IO(new Bundle {
    val enable = Input(Vec(DstNum, Bool()))
    val req = Input(Vec(SrcNum, Vec(DstNum, Bool())))
    val gnt = Output(Vec(SrcNum, Vec(DstNum, Bool())))
  })
  val src_ptr = RegInit(VecInit(Seq.fill(SrcNum)(0.U(DstNum.W))))
  val dst_ptr = RegInit(VecInit(Seq.fill(DstNum)(0.U(SrcNum.W))))
  val src_ptrx = RegInit(0.U(3.W))
  val mask_gnt = Wire(Vec(SrcNum,Vec(DstNum,Bool())))

  private val U_ISLIP_LOGIC = Module(new iSlipLogic(SrcNum, DstNum))
  U_ISLIP_LOGIC.io.src_ptr := src_ptr
  U_ISLIP_LOGIC.io.dst_ptr := dst_ptr
  U_ISLIP_LOGIC.io.req     := io.req
  for(s<-0 until(SrcNum)){
    for(d<-0 until DstNum){
      mask_gnt(s)(d) := Mux(io.enable(d), U_ISLIP_LOGIC.io.gnt(s)(d),false.B)
    }
  }
  io.gnt := mask_gnt

  src_ptrx := src_ptrx+1.U
  for (s <- 0 until SrcNum) {
    when(mask_gnt(s).reduceTree(_ | _)) {
      src_ptr(s) := io.gnt(s).asUInt
    }
  }

  for (d <- 0 until DstNum) {
    when(mask_gnt.map(s => s(d)).reduce(_ | _)) {
    //when(io.enable(d)){
      dst_ptr(d) := Seq2Vec(io.gnt.map(s => s(d))).asUInt
    }
  }
}





