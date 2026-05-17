package FPP.Parser
import BaseCbb.GenBundle
import chisel3._
import chisel3.util.{is, switch}
class Pho(PhoNum:Int,PhoWidth:Int) extends GenBundle{
  val offset = Vec(PhoNum,UInt(PhoWidth.W))
}

class Phi extends GenBundle{
  val part0 = new PhiPart0
}

class PhiPart0 extends GenBundle{
  val v = UInt(16.W)
}

object extractHeader{
  def apply(in:UInt,v:GenBundle):GenBundle={
    in(in.getWidth-1,in.getWidth-v.getWidth).asTypeOf(v)
  }
}

class ParserFunction {
  def parserEthernet(in: UInt, offset: UInt,pho:Pho,phi:Phi):(UInt,Pho,Phi)={
    val etherHeader = extractHeader(in,new ethernet).asTypeOf(new ethernet)
    // Note: VLAN parsing is now handled in ParserCore via parseVlan object
    (in, pho, phi)
  }
}