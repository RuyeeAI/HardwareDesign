package Perf.common

import scala.collection.mutable.ArrayBuffer
case class PktInfo(
                      packetLength:Int = 64,
                      ecn:Int          = 0,
                      ii:Int           = 0,
                      hint:Int         = 0,
                      )

/*
class CaqmSwitch {
  var Beta    = 0
  val PortNum = 1
  val Bw        = new Array[Int](PortNum)
  var CC        = new Array[Int](PortNum)
  var DC        = new Array[Int](PortNum)
  var Queue     :Array[ArrayBuffer[PktInfo]] = Array()
  var txBytes   :Array[Int] = Array()
  def updatePktDeq(pkt:PktInfo,portID:Int) ={
    var ECN = 0
    var II  = 0

    if(pkt.ecn==1){
      CC(portID) = CC(portID) + Beta
    }else if(CC(portID)>pkt.ii){
      CC(portID) = CC(portID) - pkt.ii
      II  = pkt.ii
    }else if(CC(portID)>=0){
      val u = math.random()
      if(u<pkt.hint){
        ECN = 1
        DC(portID) += Beta
      }else{
        if(DC(portID)>pkt.ii){
          DC(portID)-=pkt.ii
        }else{
          II = 0
        }
      }
    }else{
      CC(portID) +=Beta
      ECN = 1
    }
  }

  def updatePerPeriod(Period:Int,Bw:Array[Int],portID:Int)={
    val maxDataSize = Bw(portID)*Period
    val freeDataSize = maxDataSize - txBytes(portID) + QueueTarget - sum(QueueFillLevel) - creditAllocated

    CC = freeDataSize*lambda
    txBytes = 0
    DC = 0
    creditAllocated = 0
  }


}
*/