package Perf.FPP
import Perf.common._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Queue}
import scala.math.{ceil, random}
case class PfcPacket(
                      abs_duration:Int,
                      vl_xoff:Boolean
                    )

class AbsPfcTxDevice (p:PacketGeneratorPara)    {
  var pfcTimer :Int = 0
  var cellQueue:mutable.Queue[PktCell]=mutable.Queue()
  var pktQueue:Queue[Int] = mutable.Queue()
  val shaper = new Shaper(p.bucketDep , p.Bandwidth, p.Freq )
  var CreditCounter:Int = 10000
  var CreditResolution:Int = 8
  var CreditPreDec:Int = 0

  def ready(): Boolean = {
    val lbo = if(cellQueue.nonEmpty)cellQueue.head.lbo else 0
    shaper.bucket > 0 && (CreditCounter >= (math.ceil(lbo / CreditResolution).toInt + CreditPreDec)) && cellQueue.nonEmpty
  }

  def init(): Unit = {
    pktQueue = GenPacketQueue(p.dist, p.packetNum)
    cellQueue = GenPacketCellQueue(pktQueue, p.busSize)
    shaper.init()
    CreditCounter = p.initCreditCounter
    CreditPreDec = p.creditPreDec
    CreditResolution = p.creditRes
  }

  def GenPacketQueue(d: PacketDistribution, packetNum: Int): mutable.Queue[Int] = {
    var q: Queue[Int] = Queue()
    var keys = d.dist.keys.toSeq
    var distStep: ArrayBuffer[Double] = ArrayBuffer()
    for (i <- 0 until (keys.size)) {
      if (i == 0) {
        distStep += d.dist(keys(i))
      } else {
        distStep += distStep(i - 1) + d.dist(keys(i))
      }
    }

    distStep = distStep.map(x => x / d.dist.values.sum)
    for (i <- 0 until packetNum) {
      if (d.mode == "Random") {
        val size = ceil(random() * (d.sizeRange(1) - d.sizeRange(0)) + d.sizeRange(0)).toInt
        q.enqueue(size)
      } else {
        val r = random()
        var size = keys.last
        for (i <- keys.indices) {
          if (r < distStep(keys.size - 1 - i)) {
            size = keys(keys.size - 1 - i)
          }
        }
        q.enqueue(size)
      }
    }
    q
  }

  def GenPacketCellQueue(pktQueue: mutable.Queue[Int], BusSize: Int = 256): mutable.Queue[PktCell] = {
    val pktNum = pktQueue.size
    var cellQueue: mutable.Queue[PktCell] = mutable.Queue()
    for (i <- 0 until pktNum) {
      var size = pktQueue.dequeue()
      if (size < BusSize) {
        cellQueue.enqueue(PktCell(true, true, size))
      } else {
        cellQueue.enqueue(PktCell(true, false, BusSize))
        size = size - BusSize
        while (size > 256) {
          cellQueue.enqueue(PktCell(false, false, BusSize))
          size = size - BusSize
        }
        cellQueue.enqueue((PktCell(false, true, size)))
      }
    }
    cellQueue
  }


  def ReceivePfcDuration(pkt:PfcPacket)= {
    if (pkt.abs_duration > 0) {
      pfcTimer = pkt.abs_duration
    } else if (pfcTimer > 0) {
      pfcTimer = pfcTimer - 1
    }
    var bp = false
    if(pfcTimer>0 || pkt.vl_xoff){
      bp = true
    }
    bp
  }

  def SendCell(bp: Boolean, crdtAck: Int): PktCell = {
    val lbo = if(cellQueue.nonEmpty) cellQueue.head.lbo else 0
    if (cellQueue.nonEmpty && shaper.bucket > 0 && !bp) {
      val cell = cellQueue.dequeue()
      //println("Before update "+shaper.bucket.toInt )
      shaper.ShaperUpdate(cell.lbo)
      //println("After update "+shaper.bucket.toInt)
      cell
    } else {
      shaper.ShaperUpdate(0)
      PktCell(sop = false, eop = false, 0)
    }

  }

}

class absPfcRxDevice(OutPutBw:Int,AbsDur:Int,AbsTh:Int) {
  var RxBuf:ArrayBuffer[PktCell] = ArrayBuffer()
  var shaper = new Shaper(1000,OutPutBw,1.2)
  var timer:Int = 0
  var last_time = 0
  def init()={
    shaper.init()
  }



  def step(in:PktCell)={
    var pfc = PfcPacket(0,false)
    timer = timer+1
    if(timer == AbsDur){
      timer = 0
      if(RxBuf.length>AbsTh) {
        if(RxBuf.length>last_time){
          pfc = PfcPacket(RxBuf.length -  last_time, true)
          last_time = RxBuf.length - ( last_time-AbsDur)
        }else{
          pfc = PfcPacket(RxBuf.length ,true)
          last_time = RxBuf.length
        }


        println("INFO:Generate ABS_PFC packet from RX device!!! "+RxBuf.length)
      }else{
        pfc = PfcPacket(0,false)
      }
    }else{
      pfc = PfcPacket(0,false)
    }

    if(in.lbo>0){
      RxBuf.append(in)
    }

    if(shaper.bucket>0 && RxBuf.length>0){
      shaper.ShaperUpdate(RxBuf.head.lbo)
      RxBuf.remove(0)
    }else{
      shaper.ShaperUpdate(0)
    }

    pfc
  }
}

case class absCfg(
                 runtime:Int = 10000,
                 txCfg: PacketGeneratorPara,
                 RxOutBw : Int,
                 AbsDur  : Int,
                 AbsTh   : Int,
                 CableLen: Int
                 )

class AbsSystem(cfg:absCfg){
  val TxDev    = new AbsPfcTxDevice(cfg.txCfg)
  val T2RCable = new Delayline(cfg.CableLen)
  val RxDev    = new absPfcRxDevice(cfg.RxOutBw,cfg.AbsDur,cfg.AbsTh)
  val R2TCable = new Delayline(cfg.CableLen)
  import java.io._
  val writer = new PrintWriter(new File("generated/abs_sim.csv"))
  writer.write("cycle,TxSendLen,TxShaper,RxReceLen,RxShaper,RxBufDep,PfcDuration,XoffSender\n")
  def init()={
    TxDev.init()
    RxDev.init()
    T2RCable.init()
    R2TCable.init()
  }

  def main()={
    var new_cell:PktCell = PktCell(false,false,0,0)
    var T2R :PktCell = PktCell(false,false,0,0)
    var R2T :PfcPacket=PfcPacket(0,false)
    var R2T_D :PktCell=PktCell(false,false,0,0)
    var bp = false
    for(i<-0 until cfg.runtime){

      new_cell = TxDev.SendCell(bp,0)
      T2R      = T2RCable.step(new_cell)
      R2T      = RxDev.step(T2R)
      R2T_D    = R2TCable.step(PktCell(false,false,0,0,R2T))
      bp       = TxDev.ReceivePfcDuration(R2T_D.pfc)
      val bp_int = if(bp) 1 else 0
      writer.write(i+","+new_cell.lbo+","+TxDev.shaper.bucket+","+T2R.lbo+","+RxDev.shaper.bucket+","+RxDev.RxBuf.length+","+R2T.abs_duration+","+bp_int+"\n")
      //printf("cycle %4d TxDev Send "+new_cell+ "TX Shaper "+TxDev.shaper.bucket+"  RxDev receive "+T2R+" Buf Dep: "+RxDev.RxBuf.length+" RX Shaper "+RxDev.shaper.bucket+ " \n",i)

    }
  }

}

object absTest extends App{
  val cfg = absCfg(
    runtime = 150000,
    txCfg = PacketGeneratorPara(Bandwidth = 1600, Freq = 1.2, bucketDep = 1000 , dist = PacketDistribution(mode = "Random", sizeRange = Array(9600,9600)),packetNum = 3000 ),
    RxOutBw = 1500,
    AbsDur = 1000,
    AbsTh = 10,
    CableLen = ((2000/(3e8*0.69))*1e9*1.2).toInt
  )
  println(cfg.CableLen)
  val g = new AbsSystem(cfg)
  g.init()
  g.main()

}

