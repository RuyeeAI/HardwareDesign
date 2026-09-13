package Perf.FPP

import Perf.common._

import scala.collection.mutable.{ArrayBuffer, Queue}
import scala.collection.mutable

case class PortIdCmd(valid:Boolean,PortId:Int,PKtSizeChange:Int)
class PortCmdDelayline (latency:Int){
  var d = new Array[PortIdCmd](latency)
  def init()={
    for(i<-0 until(latency)){
      d(i) = PortIdCmd(valid = false,0,0)
    }
  }
  def step(in:PortIdCmd)={
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


class NwAligner(MacBusSize:Int){
  var tail:mutable.Queue[PktCell] = mutable.Queue()
  def step(in:PktCell)={
    var MacOut:PktCell = PktCell(sop = false,eop = false, lbo = 0, port =0)
    if(tail.isEmpty){
      if(in.lbo>MacBusSize){
        MacOut = PktCell(sop=in.sop,eop = false,lbo=MacBusSize,port =in.port)
        tail.enqueue(PktCell(sop=false,eop=in.eop,lbo = in.lbo-MacBusSize,port = in.port))
      }else{
        MacOut = in
      }
    }else if(tail.size == 1){
      MacOut = tail.dequeue()
      if(in.lbo>MacBusSize){
        tail.enqueue(PktCell(sop = in.sop,eop = false,lbo = MacBusSize,port = in.port))
        tail.enqueue(PktCell(sop = false ,eop = in.eop,lbo = in.lbo-MacBusSize,port = in.port))
      }else{
        tail.enqueue(in)
      }
    }else if (tail.size == 2){
      MacOut = tail.dequeue()
      if(in.lbo>MacBusSize){
        println("Error:Tail register overflow")
      }else{
        tail.enqueue(in)
      }
    }
    MacOut
  }
}

class EpmSch(ready:Array[Boolean]){

}

class ETB{
  var PortQ:Queue[PktCell] = Queue()
  PortQ.enqueue(PktCell(false,false,12))
}
class EPPLane (PortNum:Int,eppLat:Int){
  var epp_cp = new PortCmdDelayline(eppLat)
  var etb: ArrayBuffer[ETB] = ArrayBuffer()
  var hcib: ArrayBuffer[Queue[PortIdCmd]] = ArrayBuffer()
  var nw_alg0 = new NwAligner(152)
  var nw_alg1 = new NwAligner(152)
  var nw_sch  = new TDM_WRR(Array(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),Array(16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16),maxWeight = 32)
  //var etb_rsv:Array[Int] = Array(PortNum)
  def req ={
    var req:Array[Boolean] = new Array(PortNum)
    for(i<-0 until PortNum){
      if(etb(i).PortQ.nonEmpty && etb(i).PortQ.head.sop){
        req(i) = hcib(i).nonEmpty
      }else if(etb(i).PortQ.nonEmpty){
        req(i) = true
      }else{
        req(i) = false
      }
    }
    req
  }

  def init()={
    epp_cp.init()
    for(i<-0 until(PortNum)) {
      val emptyETB = new ETB()
      val emptyQueue:Queue[PortIdCmd] = mutable.Queue()
      etb.append(emptyETB)
      hcib.append(emptyQueue)
    }
  }

  /**
   *
   * @param in the cell written into ETB.
   * @param PktSizeChange
   * @return
   */
  def step(in:PktCell,PktSizeChange:Int) = {
    //Packet Cell written into ETB
    println(etb(0).PortQ.isEmpty)
    etb(in.port).PortQ.enqueue(PktCell(sop=in.sop,eop =  in.eop,lbo = in.lbo,port = in.port))
    //Packet after EPP CP, assume EPP CP is fixed latency. & CP processing result contains port ID, packet size change bytes
    val hcib_cmd_in = epp_cp.step(PortIdCmd(in.lbo>0,in.port,PktSizeChange))
    hcib(hcib_cmd_in.PortId).enqueue(hcib_cmd_in)
    val sch_req = req
    val sch_gnt = nw_sch.step(sch_req)
    val sch_valid = sch_gnt._1
    val sch_port  = sch_gnt._2

////    val sch_valid = true
//    val sch_port  = 0
    var mac0_out:PktCell = PktCell(false,false,0,0)
    var mac1_out:PktCell = PktCell(false,false,0,0)
    val etb_out = PktCell(true,true,256)
    if(sch_valid){
      //val etb_out = etb(sch_gnt._2).dequeue._1
      if(sch_port<8){
        mac0_out = nw_alg0.step(etb_out)
        mac1_out = nw_alg1.step(PktCell(false,false,0,0))
      }else{
        mac0_out = nw_alg0.step(PktCell(false,false,0,0))
        mac1_out = nw_alg1.step(etb_out)
      }
    }else{
      mac0_out = nw_alg0.step(PktCell(false,false,0,0))
      mac1_out = nw_alg1.step(PktCell(false,false,0,0))
    }
    (mac0_out,mac1_out)
  }
}
