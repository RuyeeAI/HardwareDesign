package Perf.common

import Perf.FPP.PfcPacket

import scala.collection.mutable
import scala.collection.mutable._
import scala.collection.mutable.ArrayBuffer
import scala.math.{ceil, random, sin}

case class PacketGeneratorPara(
                                Bandwidth:Double = 100,
                                Freq:Double = 1.2,
                                bucketDep:Double = 1000,
                                dist:PacketDistribution = PacketDistribution(sizeRange = Array(408,408)),
                                packetNum:Int = 1000,
                                busSize:Int = 256,
                                initCreditCounter:Int = 1400,
                                creditRes:Int = 1,
                                creditPreDec:Int = 0
                              )


class PacketGenerator(p:PacketGeneratorPara) {
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

  def SendCell(bp: Boolean = false, crdtAck: Int = 0): PktCell = {
    val lbo = if(cellQueue.nonEmpty) cellQueue.head.lbo else 0
    val creditEnough = CreditCounter >= (math.ceil(lbo / CreditResolution).toInt + CreditPreDec)
    if (cellQueue.nonEmpty && shaper.bucket > 0 && creditEnough && !bp) {
      CreditCounter = CreditCounter - (math.ceil(cellQueue.head.lbo / CreditResolution).toInt + CreditPreDec) + crdtAck
      val cell = cellQueue.dequeue()
      shaper.ShaperUpdate(cell.lbo)
      cell
    } else {
      CreditCounter = CreditCounter + crdtAck
      shaper.ShaperUpdate(0)
      PktCell(sop = false, eop = false, 0)
    }
  }
}

class MultiPortPktGenerator(p:Array[PacketGeneratorPara]) {
  val port = for(i<- p.indices) yield{
    new PacketGenerator(p(i))
  }

  def init()={
    p.indices.map(x=>port(x).init())
  }
  def ready():Array[Boolean]={
    p.indices.map(x=>port(x).ready()).toArray
  }
  def SendCell(en:Boolean,Id:Int,crdtAck: Array[Int]):PktCell={
    val a = p.indices.map(x=>port(x).SendCell(!en || Id!=x,crdtAck(x)))
    if(en){
      a(Id)
    }else{
      PktCell(sop = false,eop = false,0)
    }

  }
}

case class PktCell(
                  sop:Boolean = true,
                  eop:Boolean = true,
                  lbo:Int = 64,
                  port:Int = 0,
                  pfc:PfcPacket = PfcPacket(0,vl_xoff = false)
                  )

case class PacketDistribution(
                             mode:String = "Random", // Random or Dist
                             sizeRange:Array[Int] = Array(64,64), // used when mode is Random,
                             dist:Map[Int,Int] = Map(64->1,9600->1) //used when mode is Dist
                             )

