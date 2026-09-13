package Perf.common

// 原文件此处有 `import Perf.common.test.rr`，该符号在仓库中不存在（编译不过）；
// 且全文未引用，删除即可。
class RR {
  var pointer:Int = 0
  var select:Int = 0
  def step(in:Array[Boolean])={
    var sel = false
    for(i<- in.length-1 to pointer by -1){
      if(in(i)){
        sel = true
        select = i
      }
    }
    if(!sel){
      for(i<-pointer-1 to 0 by -1){
        if(in(i)){
          sel = true
          select = i
        }
      }
    }
    if(sel){
      if(select==in.length-1){
        pointer = 0
      }else{
        pointer = select+1
      }

    }
    (sel,select)
  }
}


class WRR(WeightCfg:Array[Int],MaxWeight:Int=256){
  var weight:Array[Int] = Array()
  var weightCfg:Array[Int] = Array()
  val RR = new RR
  def init(): Unit ={
    weight = WeightCfg
    weightCfg = WeightCfg.map(x=>x)
  }
  def step(in:Array[Boolean]): (Boolean, Int) ={
    if(!in.zip(weight).map(x=>x._1 && x._2>0).reduce(_|_) && in.reduce(_|_)){
      weight = weight.zip(weightCfg).map(x=>math.min(MaxWeight,x._1+x._2))
    }
    val res = RR.step(in.zip(weight).map(x=>x._1 && x._2>0))
    if(res._1){
      weight(res._2) = weight(res._2)-1
    }
    res
  }
}

class TDM(cal:Array[Int]){
  var t = 0
  var sel = 0
  def step()={
    sel = cal(t)
    t=(t+1)%cal.length
    sel
  }
}


class TDM_WRR(cal:Array[Int],weightCfg:Array[Int],maxWeight:Int){
  val tdm = new TDM(cal=cal)
  val wrr = new WRR(WeightCfg = weightCfg, MaxWeight = maxWeight)
  def init()={
    wrr.init()
  }

  def step(req:Array[Boolean])={
    val tdm_sel = tdm.step()
    if(!req(tdm_sel)){
      val r = wrr.step(req)
      r
    }else{
      (true,tdm_sel)
    }

  }
}
object test extends App{
  val rr = new TDM_WRR(cal=Array(0,1,2,3),Array(3,2,1,1),8)
  rr.init()
  for(i<-0 until 100){
    println(rr.step(Array(true,true,false,true)))
  }
}