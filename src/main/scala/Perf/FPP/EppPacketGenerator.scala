package Perf.FPP
import Perf.common._

case class EppPacketGeneratorCfg(
  portCfg:Array[PacketGeneratorPara]= Array(
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
    PacketGeneratorPara(),
  ),
  schTdmCal:Array[Int] = Array(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
  schWrrWeightCfg:Array[Int] = Array(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
  schWrrMaxWeight:Int = 10,
)


class EppPacketGenerator(cfg:EppPacketGeneratorCfg) {
  val ports = new MultiPortPktGenerator(cfg.portCfg)
  val sch = new TDM_WRR(cfg.schTdmCal,cfg.schWrrWeightCfg,cfg.schWrrMaxWeight)
  def init():Unit={
    ports.init()
    sch.init()
  }

  def step(crdtAck:Array[Int]):PktCell={
    val sel  = sch.step(ports.ready())
    val cell = ports.SendCell(sel._1,sel._2,crdtAck)
    PktCell(cell.sop,cell.sop,cell.lbo,sel._2)
  }
}



