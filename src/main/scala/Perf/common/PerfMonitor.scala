package Perf.common

import scala.collection.mutable.ArrayBuffer

class PerfMonitor(Freq:Double,len:Int) {
  var arr:ArrayBuffer[PktCell] = ArrayBuffer()
  def monitor(in:PktCell)={
    arr = arr.append(in)
    if(arr.length>len){
      arr.remove(0)
    }
    //arr.foreach(x=>print(x.lbo+","))
    printf("Performance is %10f\n",(arr.map(x => x.lbo).sum +arr.map(x=>if(x.sop) 20 else 0).sum)*8/(len/Freq))
  }

}
