package BaseCbb.Clos
import BaseCbb.utils._
import chisel3._


class BenesClos2x2[T<:Data](dt:T) extends Module{
  val io = IO(new Bundle{
    val sel: Bool = Input(Bool())
    val in  = Input(Vec(2,dt))
    val out = Output(Vec(2,dt))
  })
  io.out := Mux(io.sel,Seq2Vec(io.in.reverse),io.in)
}

class BenesClos[T<:Data](dt:T,Num:Int) extends Module{
  val io = IO(new Bundle {
    val sel = Input(Vec(Benes.CfgSize(Num),Bool()))
    val in  = Input(Vec(Num,dt))
    val out = Output(Vec(Num,dt))
  })
  io.out := Benes.ClosNxN(io.sel,io.in)
}

object BenesClos{
  def apply[T<:Data](s:Vec[Bool],in:Vec[T]):Vec[T] = {
    val U_BenesClos = Module(new BenesClos(in.head.cloneType,in.length))
    U_BenesClos.io.sel := s
    U_BenesClos.io.in  := in
    U_BenesClos.io.out
  }
}



object Benes{
  def CfgSize(len:Int):Int={
    val upperHalf = math.floor(len.toDouble/2).toInt
    val lowerHalf = math.ceil(len.toDouble/2).toInt
    if(len>=3){
      val f = CfgSize(upperHalf.toInt)+CfgSize(lowerHalf.toInt)+upperHalf.toInt*2
      f
    }else if(len==2){
      1
    }else{
      0
    }
  }

  def Clos2x2[T<:Data](sel:Bool,in:Vec[T]):Vec[T]={
    require(in.length==2,message = "Benes Clos 2x2 input length is not 2")
    val U_BC2x2 = Module(new BenesClos2x2(in.head.cloneType))
    U_BC2x2.io.sel := sel
    U_BC2x2.io.in  := in
    U_BC2x2.io.out
  }


  /**
   *
   * @param s the configuration to
   * @param in
   * @tparam T
   * @return
   */
  def ClosNxN[T<:Data](s:Vec[Bool],in:Vec[T]):Vec[T] = {
    val length = in.length
    if(length==1){
      in
    }else if(length==2){
      Clos2x2(s(0),in)
    }else {
      val upperHalf = math.floor(length.toDouble / 2).toInt
      val lowerHalf = math.ceil(length.toDouble / 2).toInt
      println("Upper "+upperHalf +" Lower "+lowerHalf)
//      val firstStageCfg  = SubVec(s, st = 0, size = upperHalf)
      val lastStageCfg   = SubVec(s, st = s.length - upperHalf, upperHalf)
      val firstStageOut  = Wire(Vec(upperHalf,Vec(2,in.head.cloneType)))
      val lastStageOut   = Wire(Vec(upperHalf,Vec(2,in.head.cloneType)))
      val ClosNxNOut     = Wire(Vec(length,in.head.cloneType))
      val upperClosInput = Wire(Vec(upperHalf,in.head.cloneType))
      val lowerClosInput = Wire(Vec(lowerHalf,in.head.cloneType))
      val upperClosOutput = Wire(Vec(upperHalf,in.head.cloneType))
      val lowerClosOutput = Wire(Vec(lowerHalf,in.head.cloneType))

      for (i<-0 until (upperHalf)){
        firstStageOut(i):= Clos2x2(s(i),SubVec(in,i*2,2))
        upperClosInput(i) := firstStageOut(i)(0)
        lowerClosInput(i) := firstStageOut(i)(1)
        lastStageOut(i)   := Clos2x2(lastStageCfg(i),Seq2Vec(Seq(upperClosOutput(i),lowerClosOutput(i))))
        ClosNxNOut(2*i)   := lastStageOut(i)(0)
        ClosNxNOut(2*i+1) := lastStageOut(i)(1)
      }

      if(lowerHalf>upperHalf){
        lowerClosInput(lowerHalf-1) := in(length-1)
        ClosNxNOut(2*upperHalf)     := lowerClosOutput(lowerHalf-1)
      }

      if(upperHalf>1){
        val upperClosCfg = SubVec(s,st=upperHalf,CfgSize(upperHalf))
        upperClosOutput  := ClosNxN(upperClosCfg,upperClosInput)
      }else{
        upperClosOutput := upperClosInput
      }
      println(upperHalf+CfgSize(upperHalf),CfgSize(lowerHalf))
      val lowerClosCfg = SubVec(s,st = upperHalf+CfgSize(upperHalf),CfgSize(lowerHalf))
      lowerClosOutput := ClosNxN(lowerClosCfg,lowerClosInput)
      ClosNxNOut
    }
  }

}
