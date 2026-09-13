package Demo
import BaseCbb.memory._
import chisel3._
class TestSram extends Module{
  val io = IO(new Bundle {
    val mem = new SpMemoryPort(6,64)
  })

  val sram = DescribedSRAM(
    name = "DemoMem", desc = "This is test memory", size = 64, data = UInt(64.W)
  )
  io.mem.rdata :=  sram.readWrite(io.mem.addr,io.mem.wdata,io.mem.re || io.mem.we, io.mem.we)

}

