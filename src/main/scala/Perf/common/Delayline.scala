package Perf.common

class Delayline (latency:Int){
  var d = new Array[PktCell](latency)
  def init()={
    for(i<-0 until(latency)){
      d(i) = PktCell(false,false,0,0)
    }
  }
  def step(in:PktCell)={
    val out = d(latency-1)
    for(i<-0 until latency){
      if(i==(latency-1)){
        d(0) = in
      }else{
        d(latency-1-i) = d(latency-2-i)
      }
    }
    out
  }
}
