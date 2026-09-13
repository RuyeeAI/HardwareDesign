package Perf.common

class Shaper(bucketDep:Double,Bandwidth:Double,Freq:Double) {
  var bucket:Double = 0
  def init()= {
    bucket = bucketDep
  }
  def ShaperUpdate(dec:Int)={
    val inc = Bandwidth/Freq/8 // fill to bucket per cycle
    val bucket_temp = bucket + inc - dec
    //println("Shaper update:::::: inc = "+inc + " dec="+dec +"  tmp "+bucket_temp + "  Bucket " + bucket + " Bandwidth:"+Bandwidth)
    var bucket_next = bucket_temp
    if(bucket_temp > bucketDep){
      bucket_next = bucketDep
    }else{
      bucket_next = bucket_temp
    }
    bucket = bucket_next
  }
}
