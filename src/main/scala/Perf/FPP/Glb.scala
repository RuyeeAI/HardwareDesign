package Perf.FPP

import Perf.common._

import scala.collection.mutable
import scala.collection.mutable.Queue


class GlobalLoopbackBuffer{
  var BufFill: Queue[PktCell] = Queue()
  val shaper = new Shaper(1000,800,1.2)
  def step(cell:PktCell)={
    if(cell.lbo>0) {
      BufFill.enqueue(cell)
    }
    var glbOut = PktCell(false,false,0)
    if(shaper.bucket>0 && BufFill.size>0){
      glbOut = BufFill.dequeue()
    }
    shaper.ShaperUpdate(glbOut.lbo)
    glbOut
  }
}


class Glb (cfg:PacketGeneratorPara,EppLatency:Int){
  val runtime = 1000

  val gen = new PacketGenerator(cfg)
  val EPP = new Delayline(EppLatency)
  val Glb = new GlobalLoopbackBuffer

  // Init phase
  gen.init()
  EPP.init()



  def main()={
    for(i<-0 until runtime){
      val new_cell = gen.SendCell()
      val epp_out  = EPP.step(new_cell)
      val glbOut = Glb.step(epp_out)
      printf("cycle %4d Shaper %4d new Cell \t"+new_cell.lbo +"\t epp_output \t"+epp_out.lbo+"   \tGlb fill "+Glb.BufFill.size+"\t Glb output "+glbOut.lbo+" \n",i,gen.shaper.bucket.toInt)
    }
  }

}



class Glb_TC0{
  val glbPortCfg = PacketGeneratorPara(
    Bandwidth = 800,
    Freq = 1.2,
    bucketDep = 1000,
    dist = PacketDistribution(mode="Random",sizeRange = Array(64,64),dist = mutable.Map(64->1,9600->1)),
    packetNum = 1000,
    busSize = 256,
    initCreditCounter = 1400,
    creditRes = 32,
    creditPreDec = 3)

    val glb = new Glb(glbPortCfg,100)
}



object xTest extends App{
  val g = new Glb_TC0
  g.glb.main()

}




