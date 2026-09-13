package HBS.swf.common

import BaseCbb.data.GenModule
import chisel3._
import chisel3.util._
import Array._
class BusMatrix[T<:Data] (dt:T, SrcNum:Int, DstNum:Int)  extends  GenModule {
  val io = IO(new Bundle{
    val dn_in = Input(Vec(SrcNum,Valid(new BufferOutBundle(dt.getWidth,7))))
    val lt_in = Input (Vec(DstNum,Valid(dt)))
    val rt_in = Input (Vec(DstNum,Valid(dt)))
    val lt_ot = Output(Vec(DstNum,Valid(dt)))
    val rt_ot = Output(Vec(DstNum,Valid(dt)))
  })
  val bselSample = BusMatrixPara.bselSample(SrcNum,DstNum,dt.getWidth)
  val DstID      = BusMatrixPara.DstID(DstNum)
  val l2r = Wire(Vec(DstNum,Vec(SrcNum+1,Valid(dt))))
  val r2l = Wire(Vec(DstNum,Vec(SrcNum+1,Valid(dt))))
  val d2u = Wire(Vec(DstNum+1,Vec(SrcNum,Valid(new BufferOutBundle(dt.getWidth,7)))))
  d2u(0) := io.dn_in
  for(i<-0 until DstNum){
    l2r(i)(0)      := io.lt_in(i)
    r2l(i)(SrcNum) := io.rt_in(i)
    io.lt_ot(i)    := r2l(i)(0)
    io.rt_ot(i)    := l2r(i)(SrcNum)

    for(j<-0 until SrcNum){
      val U_BSE = Module(new BusSelection(dt,bselSample(i)(j),DstID(i))).suggestName("BSE_LAYER"+i.toString+"_INDEX"+j.toString)
      U_BSE.io.left_data_in  := l2r(i)(j)
      l2r(i)(j+1)            := U_BSE.io.right_data_out
      U_BSE.io.right_data_in := r2l(i)(j+1)
      r2l(i)(j)              := U_BSE.io.left_data_out
      U_BSE.io.down_data_in  := d2u(i)(j)
      d2u(i+1)(j)            := U_BSE.io.up_data_out
    }
  }

}

object BusMatrixPara{
  def bselSample(SrcNum:Int,DstNum:Int,BusWidth:Int)={
    val BusNumPerSampling = math.ceil(0.7/(320/BusWidth)).toInt
    var par= Array.ofDim[BselSample](DstNum,SrcNum)
    for(i<-0 until DstNum){
      for(j<-0 until SrcNum){
        if(i % BusNumPerSampling ==0){
          if(j % BusNumPerSampling ==0){
            par(i)(j) = BselSample(true,true,true)
          }else{
            par(i)(j) = BselSample(true,true,false)
          }
        }else{
          if(j%BusNumPerSampling ==0){
            par(i)(j) = BselSample(false,false,true)
          }else{
            par(i)(j) = BselSample(false,false,false)
          }
        }
      }
    }
    par
  }

  def DstID(DstNum:Int)={
    var par = Array.ofDim[(Int,Int)](DstNum)
    if(DstNum == 24){ // ID for Databus
      for(i<-0 until DstNum){
        if(i<12){
          par(i) = (i,i+32)
        }else{
          par(i) = (i+4,i+36)
        }
      }
    }else if(DstNum==12){
      for(i<-0 until DstNum){
        par(i) = (i,i+12)
      }
    }
    par
  }
}
