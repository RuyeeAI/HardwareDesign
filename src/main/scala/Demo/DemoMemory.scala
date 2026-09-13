package Demo

import chisel3._
import BaseCbb.data.GenMemory
import BaseCbb.memory.{Memory, MemoryAccessType}

/** IR 存储清单：统一使用 HD 的 `BaseCbb.memory.Memory`（原 GeneratorLib 的 IR Memory 已并入它）。
  * 参数名对应关系：Name→name、dt→dataType、AccessType→memoryType、InstNum→instNum。
  */
class DemoMemory extends GenMemory{
  MemoryArr += Memory(name = "ACL",dataType = UInt(32.W),depth = 256, memoryType = MemoryAccessType.TCAM, instNum = 1)
  MemoryArr += Memory(name = "FWD",dataType = UInt(32.W),depth = 512, memoryType = MemoryAccessType.SP, instNum = 8)
}
