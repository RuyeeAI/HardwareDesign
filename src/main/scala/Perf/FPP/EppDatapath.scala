package Perf.FPP

import BaseCbb.io.WriteFile
import Perf.common.{Delayline, PacketDistribution, PacketGeneratorPara, PerfMonitor, PktCell}

import java.io.FileWriter
import scala.collection.mutable

case class EppDatapathCfg (
                eppPacketGeneratorCfg: EppPacketGeneratorCfg,
                eppLatency:Int=100
                          )


class EppDatapath(cfg:EppDatapathCfg) {
  val eppPacketGenerator = new EppPacketGenerator(cfg.eppPacketGeneratorCfg)
  val EPP = new EPPLane(cfg.eppPacketGeneratorCfg.portCfg.length,cfg.eppLatency)
  //val Glb = new GlobalLoopbackBuffer
  val glboutMonitor = new PerfMonitor(Freq = 1.2,len=50)
  var crdt_ack:Array[Int] = Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  def init():Unit={
    eppPacketGenerator.init()
    EPP.init()
  }



  def step()={
    var info = "Cycle,\t"+cfg.eppPacketGeneratorCfg.portCfg.indices.map(x=>"Credit_P"+x).mkString(",") +","+cfg.eppPacketGeneratorCfg.portCfg.indices.map(x=>"ShaperBucket_P"+x).mkString(",")+","
    info += "Swb2Epp_Port,Swb2Epp_Size,Epp2Out_Port,Epp2OutSize,GlbOut_Port,GlbOut_Size\n"
    for(i<-0 until(1000)){
      val cell    = eppPacketGenerator.step(crdt_ack)
      val epp_out = EPP.step(cell,0)
      //val glb_out = Glb.step(epp_out)
      //crdt_ack = Array(0,0)
      //crdt_ack(glb_out.port) = math.ceil(glb_out.lbo/cfg.eppPacketGeneratorCfg.portCfg(glb_out.port).creditRes).toInt
      //glboutMonitor.monitor(glb_out)

      val port_credit = eppPacketGenerator.ports.port.map(x=>x.CreditCounter).mkString(",")
      val port_shaper = eppPacketGenerator.ports.port.map(x=>x.shaper.bucket.toInt).mkString(",")
      val source   = port_credit+","+port_shaper+ ","+cell.port+","+cell.lbo
      val epp_o    = ",\t"+epp_out._1.port+", \t"+epp_out._1.lbo
      //val glb_o    = ",\t"+glb_out.port+", \t"+glb_out.lbo
      //info+=i+","+source+epp_o+glb_o+"\n"
    }
    WriteFile("generated/log.csv",info)

  }

}

