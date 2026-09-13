package BaseCbb.CBFC
import BaseCbb.data.GenBundle
import chisel3._
import chisel3.util._

/** 信用流控（CBFC, Credit-Based Flow Control）发送端口上下文 —— WIP 迁入件。
  *
  * 相对原文件的适配（语义不变，仅改用 chisel 5 的正确 API）：
  * ① `if(PoolNum>0) Vec(...) else None` → 显式 `Option[Vec[UInt]]`（Bundle 可选字段）；
  * ② `Vec(VcNum, RegInit(...))` → `RegInit(VecInit(Seq.fill(...)))`（前者不是合法的寄存器声明）。
  */
class CbfcPortCfg(VcNum:Int,CcWidth: Int, CfWidth: Int, PoolNum:Int) extends GenBundle{
  val S_VC_CL = Vec(VcNum,UInt(CcWidth.W))
  val S_P_CL  = UInt(CcWidth.W)
  val S_POOL_CL: Option[Vec[UInt]] = if(PoolNum>0) Some(Vec(PoolNum,UInt(CcWidth.W))) else None
  val S_VC_POOLID: Option[Vec[UInt]] = if(PoolNum>0) Some(Vec(VcNum,UInt(log2Ceil(PoolNum).W))) else None
}

class CrdInfo (VcNum:Int,CrdWidth:Int) extends GenBundle{
  val valid   = Bool()
  val vc_id   = UInt(log2Ceil(VcNum).W)
  val vc_crd  = UInt(CrdWidth.W)
}

class CfUpdate(VcNum:Int,CfWidth:Int) extends GenBundle{
  val vld = Bool()
  val cf  = Vec(VcNum,UInt(CfWidth.W))
}

class CbfcTxPort (VcNum:Int, PoolNum:Int,CcWidth:Int,CfWidth:Int) extends Module{
  val CrdWidth = log2Ceil(9600/64)
  val io = IO(new Bundle{
    val PortCfg   = Input(new CbfcPortCfg(VcNum,CcWidth,CfWidth,PoolNum))
    val tx_info   = Input(new CrdInfo(VcNum = VcNum, CrdWidth))
    val cf_update = Input(new CrdInfo(VcNum, CfWidth))
  })

  val S_VC_CC = RegInit(VecInit(Seq.fill(VcNum)(0.U(CcWidth.W))))
  val S_VC_CU = RegInit(VecInit(Seq.fill(VcNum)(0.U(CcWidth.W))))
  val S_VC_CF = RegInit(VecInit(Seq.fill(VcNum)(0.U(CfWidth.W))))

  for(i<-0 until VcNum) {
    when(io.tx_info.valid && io.tx_info.vc_id === i.U) {
      S_VC_CC(i)  := S_VC_CC(i) + io.tx_info.vc_crd
    }

    when(io.cf_update.valid && io.cf_update.vc_id===i.U){
      S_VC_CF(i) := io.cf_update.vc_crd
    }


  }

}
