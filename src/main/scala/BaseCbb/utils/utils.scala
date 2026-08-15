package BaseCbb.utils

import chisel3._
import chisel3.util._
import chisel3.experimental._


object GenProcessBuilder{
  def apply(cmd:String):Process={
    // 按空白拆分命令与参数（修复：原实现把整条命令当作单个可执行文件路径）
    val parts = cmd.split("\\s+").filter(_.nonEmpty)
    println(s"System Command Excute: $cmd")
    val delay_process = new ProcessBuilder(parts: _*)
    delay_process.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    delay_process.redirectError(ProcessBuilder.Redirect.INHERIT)
    delay_process.start()
  }
}

object Seq2Vec{
  def apply[T<:Data](s:Seq[T]):Vec[T]={
    val v = Wire(Vec(s.length,s.head.cloneType))
    (0 until s.length).map(i=>v(i):=s(i))
    v
  }
}

object SubVec{
  def apply[T<:Data](v:Vec[T],st:Int,size:Int)={
    Seq2Vec((st until st+size).map(i=>v(i)))
  }
}

object Convert2dArray{
  def apply[T<:Data](v:Vec[Vec[T]]):Vec[Vec[T]] = {
    val cv = Wire(Vec(v.head.length,Vec(v.length,v.head.head.cloneType)))
    for(i<-0 until v.length){
      for(j<-0 until v.head.length){
        cv(j)(i) := v(i)(j)
      }
    }
    cv
  }
}
// 注：原 ShiftRegEn 已并入 utils.ShiftRegInit（新增 en 重载），本文件不再保留。
