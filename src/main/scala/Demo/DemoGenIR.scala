package Demo

import BaseCbb.data.GenIR
import BaseCbb.io.JsonTools

class DemoGenIR extends GenIR{
  val Param = new DemoParam
  val DataStruct = new DemoDataStruct
  val MemoryList = new DemoMemory
}

object Main extends App{
  val d = new DemoGenIR
  // 产物统一写 generated/（已 gitignore，可随时重新生成）
  JsonTools.saveMap2JsonFile("generated/DemoIR.json",d.toIR)

}
