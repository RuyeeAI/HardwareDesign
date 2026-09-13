package Demo

import BaseCbb.data.GenParam

class DemoParam extends GenParam{
  val CoreFreq = 1200
  Desc +=("CoreFreq" -> "Work Freq or the block")

  val Pps = 1200
  Desc +=("Pps" -> "packet per second per pipeline")

  val PortNum = 144
  Desc +=("PortNum"->"Port Num")
}

