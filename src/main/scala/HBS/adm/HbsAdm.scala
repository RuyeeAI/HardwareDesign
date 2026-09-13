package HBS.adm

import BaseCbb.data.{GenBundle, GenModule}
import chisel3._
import chisel3.util.log2Ceil

class HbsAdm extends GenModule {
  val io = IO(new Bundle{

  })


  ///Packet

}


class PacketInterface (val LboW:Int = 8, val PortNum:Int = 17) extends GenBundle{
  val valid = Bool()
  val sop   = Bool()
  val eop   = Bool()
  val err   = Bool()
  val port  = UInt(log2Ceil(PortNum).W)
  val lbo   = UInt(LboW.W)
}

class PacketAggregatorPerLane (val PortNum:Int=17) extends GenModule{
  val io = IO(new Bundle{
    val pkt_in = Input(new PacketInterface())
  })

  val packetLen = VecInit(Seq.fill(PortNum)(RegInit(0.U(14.W))))

  for(i<-0 until(PortNum)){
    when(io.pkt_in.valid && io.pkt_in.port ===i.asUInt){
      when(io.pkt_in.eop){
        packetLen(i) := 0.U
      }.otherwise{
        packetLen(i) := packetLen(i) + Mux(io.pkt_in.eop,io.pkt_in.lbo+1.U,256.U)
      }
    }
  }




}