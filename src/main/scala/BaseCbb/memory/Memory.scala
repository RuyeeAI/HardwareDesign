package BaseCbb.memory
import chisel3._
import chisel3.experimental._
import chisel3.util.{Pipe, RegEnable, log2Ceil}
import BaseCbb.GenBundle

case class Memory(
                 name:String,
                 dataType:Data,
                 depth:Int,
                 memoryType:String = "1RW",
                 flopIn:Boolean=true,
                 flopOut:Boolean=true,
                 protect:String = "ECC",
                 protectWidThre:Int = 320
                 ) {

  def dataWidth:Int = {
    val eccSegNum = math.ceil(dataType.getWidth.toDouble / protectWidThre).toInt
    if(protect=="ECC") {
      val eccSegWidth = math.ceil(dataType.getWidth / eccSegNum).toInt
      val lastEccSegWidth = dataType.getWidth - (eccSegNum - 1) * eccSegWidth
      val eccTotalWidth = eccWidth(eccSegWidth) * (eccSegNum - 1) + eccWidth(lastEccSegWidth)
      eccTotalWidth + dataType.getWidth
    }else if(protect == "Parity"){
      dataType.getWidth + eccSegNum
    }else{
      dataType.getWidth
    }
  }

  def latency :Int = {
    var lat = 1
    if(flopIn){
      lat = lat+1
    }
    if(flopOut){
      lat = lat+1
    }
    lat
  }

  def eccWidth(n:Int):Int={
    val k = log2Ceil(n)
    if(math.pow(2,k)>=(n+k+1)){
      k
    }else{
      k+1
    }
  }

  def addrWidth:Int = log2Ceil(depth)
}

class SpMemoryPort(val addrWidth:Int,val dataWidth:Int) extends GenBundle {
  val we = Input(Bool())
  val re = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val wdata = Input(UInt(dataWidth.W))
  val rdata = Output(UInt(dataWidth.W))
}

class TpMemoryPort(val addrWidth:Int,val dataWidth:Int) extends GenBundle {
  val we = Input(Bool())
  val re = Input(Bool())
  val waddr = Input(UInt(addrWidth.W))
  val raddr = Input(UInt(addrWidth.W))
  val wdata = Input(UInt(dataWidth.W))
  val rdata = Output(UInt(dataWidth.W))
}

class SpMemoryBB(mem:Memory) extends BlackBox{
  val io = IO(new Bundle{
    val clk   = Input(Clock())
    val we    = Input(UInt(1.W))
    val re    = Input(UInt(1.W))
    val addr  = Input(UInt(mem.addrWidth.W))
    val wdata = Input(UInt(mem.dataWidth.W))
    val rdata = Output(UInt(mem.dataWidth.W))
  })
}

class TpMemoryBB(mem:Memory) extends BlackBox{

  val io = IO(new Bundle{
    val clk   = Input(Clock())
    val we    = Input(UInt(1.W))
    val re    = Input(UInt(1.W))
    val waddr = Input(UInt(mem.addrWidth.W))
    val raddr = Input(UInt(mem.addrWidth.W))
    val wdata = Input(UInt(mem.dataWidth.W))
    val rdata = Output(UInt(mem.dataWidth.W))
  })
}

class SimMemory (mem:Memory) extends Module{
  val io = IO(new TpMemoryPort(mem.addrWidth,mem.dataType.getWidth))
  val m = Reg(Vec(mem.depth,UInt(mem.dataType.getWidth.W)))
  val we = Wire(Bool())
  val wdata = Wire(UInt(mem.dataType.getWidth.W))
  val waddr = Wire(UInt(mem.addrWidth.W))
  val raddr = Wire(UInt(mem.addrWidth.W))
  val re    = Wire(Bool())
  if(mem.flopIn){
    we    := RegNext(io.we)
    waddr := RegEnable(io.waddr,io.we)
    wdata := RegEnable(io.wdata,io.we)
    raddr := RegEnable(io.raddr,io.re)
    re    := RegNext(io.re)
  }else{
    we := io.we
    re := io.re
    waddr := io.waddr
    wdata := io.wdata
    raddr := io.raddr
  }

  when(we){
    m(waddr) := wdata
  }

  if(mem.flopOut) {
    io.rdata := RegNext(RegNext(m(raddr)))
  }else{
    io.rdata := RegNext(m(raddr))
  }

}

class MemoryWrap extends RawModule{
  //Change the memory to Simulation or Physical memory
  def MEM_TYPE = "SIMULATION"
}

class SpMemoryWrap(mem:Memory) extends MemoryWrap {
  val clk = IO(Input(Clock()))
  val lgc = IO(new SpMemoryPort(mem.addrWidth,mem.dataType.getWidth))

  if(MEM_TYPE!="SIMLUATION") {
    val mem_inst = Module(new SpMemoryBB(mem)).suggestName(mem.name + "_PHY_MEM")
    mem_inst.io.clk := clk
    mem_inst.io.we := lgc.we
    mem_inst.io.re := lgc.re
    mem_inst.io.addr := lgc.addr
    mem_inst.io.wdata := lgc.wdata
    lgc.rdata := mem_inst.io.rdata
  }else{
    withClock(clk) {
      val mem_inst = Module(new SimMemory(mem))
      mem_inst.io.re := lgc.re
      mem_inst.io.we := lgc.we
      mem_inst.io.waddr := lgc.addr
      mem_inst.io.wdata := lgc.wdata
      mem_inst.io.raddr := lgc.addr
      lgc.rdata := mem_inst.io.rdata
    }
  }
}

class TpMemoryWrap(mem:Memory) extends MemoryWrap {
  val clk = IO(Input(Clock()))
  val rst_n = IO(Input(Bool()))
  val lgc = IO(new TpMemoryPort(mem.addrWidth,mem.dataType.getWidth))

  if(MEM_TYPE!="SIMULATION") {
    val mem_inst = Module(new TpMemoryBB(mem)).suggestName(mem.name + "_PHY_MEM")
    mem_inst.io.clk := clk
    mem_inst.io.we := lgc.we
    mem_inst.io.re := lgc.re
    mem_inst.io.waddr := lgc.waddr
    mem_inst.io.raddr := lgc.raddr
    mem_inst.io.wdata := lgc.wdata
    lgc.rdata := mem_inst.io.rdata
  }else{
    withClockAndReset(clk,rst_n) {
      val mem_inst = Module(new SimMemory(mem))
      mem_inst.io <> lgc
    }
  }
}