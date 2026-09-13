package Demo

import chisel3._
import chisel3.util.log2Ceil
import BaseCbb.data.{GenBundle, GenDataStructure}

class DestInfo (val PortNum:Int)extends GenBundle{
  val portBitmap     = UInt(PortNum.W)
  val bestPathBitmap = UInt(PortNum.W)
  val arnPort        = UInt(log2Ceil(PortNum).W)
  val arnPortValid   = Bool()
  val frnPort        = UInt(log2Ceil(PortNum).W)
  val frnPortValid   = Bool()
}

class TestBundle extends GenBundle{
  val or = UInt(3.W)

}
class DemoDataStruct extends GenDataStructure{
  val p = new DemoParam
  DataStructMap += ("DestInfo"-> new DestInfo(p.PortNum))
  DataStructMap += ("Test"-> new TestBundle)
  DataStructMap += ("Test2"-> new DemoInfoBundle(4,new DestInfo(3)))
}

class DemoInfoBundle(PortNum:Int,InfoType:GenBundle) extends GenBundle{
  val port = if(PortNum>1) Some(UInt(2.W)) else None
  val info = InfoType
}
