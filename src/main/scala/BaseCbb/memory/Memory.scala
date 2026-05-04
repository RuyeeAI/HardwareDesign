package BaseCbb.memory
import chisel3._
import chisel3.experimental._
import chisel3.util.{Cat, Pipe, RegEnable, log2Ceil}
import BaseCbb.GenBundle

case class Memory(
                 name:String,
                 dataType:Data,
                 depth:Int,
                 memoryType:String = "1RW",
                 flopIn:Boolean=false,
                 flopOut:Boolean=true,
                 protect:String = "ECC",
                 CheckIn:Boolean=false,
                 CheckOut:Boolean=true,
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

/**
 * SpMemoryLgcPort — 单口 SRAM 逻辑端口（含 ECC 不可纠正错误上报）
 */
class SpMemoryLgcPort(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val we     = Input(Bool())
  val re     = Input(Bool())
  val addr   = Input(UInt(addrWidth.W))
  val wdata  = Input(UInt(dataWidth.W))
  val rdata  = Output(UInt(dataWidth.W))
  val uecErr = Output(Bool())
}

/**
 * MemoryDfxPort — Memory 初始化控制与 ECC 错误状态接口
 */
class MemoryDfxPort(addrWidth: Int) extends GenBundle {
  val init       = Input(Bool())
  val initDone   = Output(Bool())
  val eccErr     = Output(Bool())               // 单比特错误（已纠正）
  val eccUerr    = Output(Bool())               // 不可纠正错误（双比特）
  val eccErrAddr = Output(UInt(addrWidth.W))   // eccErr 或 eccUerr 发生时的读地址
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

class SimMemory(mem: Memory) extends Module {
  val io = IO(new TpMemoryPort(mem.addrWidth, mem.dataType.getWidth))

  val m     = Reg(Vec(mem.depth, UInt(mem.dataType.getWidth.W)))
  val we    = Wire(Bool())
  val wdata = Wire(UInt(mem.dataType.getWidth.W))
  val waddr = Wire(UInt(mem.addrWidth.W))
  val raddr = Wire(UInt(mem.addrWidth.W))
  val re    = Wire(Bool())

  if (mem.flopIn) {
    we    := RegNext(io.we)
    waddr := RegEnable(io.waddr, io.we)
    wdata := RegEnable(io.wdata, io.we)
    raddr := RegEnable(io.raddr, io.re)
    re    := RegNext(io.re)
  } else {
    we    := io.we
    re    := io.re
    waddr := io.waddr
    wdata := io.wdata
    raddr := io.raddr
  }

  when(we) {
    m(waddr) := wdata
  }

  if (mem.flopOut) {
    io.rdata := RegNext(RegNext(m(raddr)))
  } else {
    io.rdata := RegNext(m(raddr))
  }
}

class MemoryWrap extends RawModule{
  //Change the memory to Simulation or Physical memory
  def MEM_TYPE = "SIMULATION"
}

/**
 * SpMemoryWrap — 单口 SRAM 封装，支持输入/输出插拍
 *
 * @param mem          Memory 配置对象（name/depth/dataType/flopIn/flopOut）
 * @param flopInDepth  输入侧插拍深度（-1 = 从 mem.flopIn 推导：1 或 0）
 * @param flopOutDepth 输出侧插拍深度（-1 = 从 mem.flopOut 推导：1 或 0）
 */
class SpMemoryWrap(
  mem:          Memory,
  flopInDepth:  Int = -1,
  flopOutDepth: Int = -1
) extends MemoryWrap {

  private val inDepth  = if (flopInDepth  >= 0) flopInDepth  else (if (mem.flopIn)  1 else 0)
  private val outDepth = if (flopOutDepth >= 0) flopOutDepth else (if (mem.flopOut) 1 else 0)

  require(inDepth  >= 0)
  require(outDepth >= 0)

  val clk   = IO(Input(Clock()))
  val rst_n = IO(Input(Bool()))
  val lgc   = IO(new SpMemoryPort(mem.addrWidth, mem.dataType.getWidth))

  // ================================================================
  // Input pipeline chain
  // ================================================================
  // we / re: RegNext × inDepth（组合旁路，仅延迟对齐）
  private val pipeInWe = (0 until inDepth).foldLeft(lgc.we)((prev, _) => RegNext(prev))
  private val pipeInRe = (0 until inDepth).foldLeft(lgc.re)((prev, _) => RegNext(prev))

  // addr / wdata: RegEnable(_, we) × inDepth（仅在 we=1 时采样）
  private val pipeInAddr  = (0 until inDepth).foldLeft(lgc.addr)((prev, _) =>
    RegEnable(prev, lgc.we))
  private val pipeInWdata = (0 until inDepth).foldLeft(lgc.wdata)((prev, _) =>
    RegEnable(prev, lgc.we))

  // ================================================================
  // Physical memory instance
  // ================================================================
  if (MEM_TYPE != "SIMULATION") {
    val mem_inst = Module(new SpMemoryBB(mem)).suggestName(mem.name + "_PHY_MEM")
    mem_inst.io.clk  := clk
    mem_inst.io.we    := pipeInWe
    mem_inst.io.re    := pipeInRe
    mem_inst.io.addr  := pipeInAddr
    mem_inst.io.wdata := pipeInWdata

    // Output pipeline
    lgc.rdata := (0 until outDepth).foldLeft(mem_inst.io.rdata)((prev, _) => RegNext(prev))

  } else {
    withClockAndReset(clk, rst_n.asBool) {
      val mem_inst = Module(new SimMemory(mem))
      mem_inst.io.we    := pipeInWe
      mem_inst.io.re    := pipeInRe
      mem_inst.io.waddr := pipeInAddr
      mem_inst.io.raddr := pipeInAddr   // read addr follows write pipeline
      mem_inst.io.wdata := pipeInWdata

      // Output pipeline
      lgc.rdata := (0 until outDepth).foldLeft(mem_inst.io.rdata)((prev, _) => RegNext(prev))
    }
  }
}

/**
 * TpMemoryWrap — 双口 SRAM 封装，支持输入/输出插拍
 *
 * @param mem          Memory 配置对象（name/depth/dataType/flopIn/flopOut）
 * @param flopInDepth  输入侧插拍深度（-1 = 从 mem.flopIn 推导：1 或 0）
 * @param flopOutDepth 输出侧插拍深度（-1 = 从 mem.flopOut 推导：1 或 0）
 */
class TpMemoryWrap(
  mem:          Memory,
  flopInDepth:  Int = -1,
  flopOutDepth: Int = -1
) extends MemoryWrap {

  private val inDepth  = if (flopInDepth  >= 0) flopInDepth  else (if (mem.flopIn)  1 else 0)
  private val outDepth = if (flopOutDepth >= 0) flopOutDepth else (if (mem.flopOut) 1 else 0)

  require(inDepth  >= 0)
  require(outDepth >= 0)

  val clk   = IO(Input(Clock()))
  val rst_n = IO(Input(Bool()))
  val lgc   = IO(new TpMemoryPort(mem.addrWidth, mem.dataType.getWidth))

  // ================================================================
  // Input pipeline chain
  // ================================================================
  // we / re: RegNext × inDepth
  private val pipeInWe = (0 until inDepth).foldLeft(lgc.we)((prev, _) => RegNext(prev))
  private val pipeInRe = (0 until inDepth).foldLeft(lgc.re)((prev, _) => RegNext(prev))

  // addr / wdata: RegEnable(_, we) × inDepth
  private val pipeInWaddr = (0 until inDepth).foldLeft(lgc.waddr)((prev, _) =>
    RegEnable(prev, lgc.we))
  private val pipeInRaddr = (0 until inDepth).foldLeft(lgc.raddr)((prev, _) =>
    RegEnable(prev, lgc.re))
  private val pipeInWdata = (0 until inDepth).foldLeft(lgc.wdata)((prev, _) =>
    RegEnable(prev, lgc.we))

  // ================================================================
  // Physical memory instance
  // ================================================================
  if (MEM_TYPE != "SIMULATION") {
    val mem_inst = Module(new TpMemoryBB(mem)).suggestName(mem.name + "_PHY_MEM")
    mem_inst.io.clk   := clk
    mem_inst.io.we    := pipeInWe
    mem_inst.io.re    := pipeInRe
    mem_inst.io.waddr := pipeInWaddr
    mem_inst.io.raddr := pipeInRaddr
    mem_inst.io.wdata := pipeInWdata

    // Output pipeline
    lgc.rdata := (0 until outDepth).foldLeft(mem_inst.io.rdata)((prev, _) => RegNext(prev))

  } else {
    withClockAndReset(clk, rst_n.asBool) {
      val mem_inst = Module(new SimMemory(mem))
      mem_inst.io.we    := pipeInWe
      mem_inst.io.re    := pipeInRe
      mem_inst.io.waddr := pipeInWaddr
      mem_inst.io.raddr := pipeInRaddr
      mem_inst.io.wdata := pipeInWdata

      // Output pipeline
      lgc.rdata := (0 until outDepth).foldLeft(mem_inst.io.rdata)((prev, _) => RegNext(prev))
    }
  }
}

/**
 * EccCodec — ECC / Parity 编解码公共函数
 */
object EccCodec {

  def eccWidthOf(segBits: Int): Int = {
    val k = log2Ceil(segBits)
    if (math.pow(2, k) >= (segBits + k + 1)) k else k + 1
  }

  def encodeParity(data: UInt, eccSegNum: Int, eccSegWidth: Int, lastEccSegWidth: Int): UInt = {
    val parityBits = VecInit((0 until eccSegNum).map { i =>
      val segBits = if (i < eccSegNum - 1) eccSegWidth else lastEccSegWidth
      val offset  = i * eccSegWidth
      val seg     = data(offset + segBits - 1, offset)
      seg.asUInt.xorR
    })
    Cat(VecInit(parityBits.reverse).asUInt, data)
  }

  def decodeParity(rdata: UInt, dataBits: Int, eccSegNum: Int, eccSegWidth: Int, lastEccSegWidth: Int): (UInt, Bool, Bool) = {
    var anyErr = false.B
    val segs = (0 until eccSegNum).map { i =>
      val segBits = if (i < eccSegNum - 1) eccSegWidth else lastEccSegWidth
      val offset  = i * eccSegWidth
      val stored  = rdata(dataBits + eccSegNum - 1, dataBits + i)
      val seg     = rdata(offset + segBits - 1, offset)
      val calc    = seg.asUInt.xorR
      anyErr      = anyErr || (stored =/= calc)
      seg
    }
    (Cat(segs.reverse), anyErr, anyErr)
  }

  def encodeEcc(data: UInt, eccSegNum: Int, eccSegWidth: Int, lastEccSegWidth: Int): UInt = {
    val segEncoded = (0 until eccSegNum).map { i =>
      val segBits = if (i < eccSegNum - 1) eccSegWidth else lastEccSegWidth
      val offset  = i * eccSegWidth
      val segData = data(offset + segBits - 1, offset)
      val k       = eccWidthOf(segBits)
      encodeEccSeg(segData, k)
    }
    Cat(segEncoded.reverse)
  }

  def encodeEccSeg(data: UInt, k: Int): UInt = {
    require(k >= 3, s"ECC segment requires at least 4 total ecc bits, got $k")
    val checkBits = Wire(Vec(k, Bool()))
    for (i <- 0 until k) {
      val pos = 1 << i
      var parity = false.B
      for (d <- 0 until data.getWidth) {
        val dPos = d + k + 1
        if ((dPos & pos) != 0) { parity = parity ^ data(d) }
      }
      checkBits(i) := parity
    }
    val dataXor       = (0 until data.getWidth).foldLeft(false.B)((p, i) => p ^ data(i))
    val checkXor      = (0 until k).foldLeft(dataXor)((p, i) => p ^ checkBits(i))
    val overallParity = checkXor
    Cat(overallParity, VecInit(checkBits.reverse).asUInt, data)
  }

  def decodeEccMultiSeg(rdata: UInt, dataBits: Int, eccSegNum: Int, eccSegWidth: Int, lastEccSegWidth: Int): (UInt, Bool, Bool) = {
    var anyErr    = false.B
    var anyUerr   = false.B
    val segDecoded = (0 until eccSegNum).map { i =>
      val segBits     = if (i < eccSegNum - 1) eccSegWidth else lastEccSegWidth
      val k           = eccWidthOf(segBits)
      val eccBitsThis = k + 1
      // Segments are reversed during encode: segOffset(i) = position of seg i's data in final layout
      // After Cat(segEncoded.reverse), seg N-1 is at low bits, seg 0 is at high bits
      val totalSegBits = segBits + eccBitsThis
      val segOffset    = (eccSegNum - 1 - i) * totalSegBits
      val rdataOffset  = segOffset + segBits  // ECC bits follow data within each segment's encoding
      val segRdata = rdata(segOffset + segBits - 1, segOffset)
      val segEcc   = rdata(rdataOffset + eccBitsThis - 1, rdataOffset)
      val (decSeg, err, uerr) = decodeEccSeg(segRdata, segEcc, k)
      anyErr  = anyErr  || err
      anyUerr = anyUerr || uerr
      decSeg
    }
    (Cat(segDecoded.reverse), anyErr, anyUerr)
  }

  def decodeEccSeg(rdata: UInt, eccFull: UInt, k: Int): (UInt, Bool, Bool) = {
    val receivedData   = rdata
    val receivedCheck  = eccFull(k - 1, 0)
    val receivedParity = eccFull(k)
    val recomputedCheck = Wire(Vec(k, Bool()))
    for (i <- 0 until k) {
      val pos = 1 << i
      var parity = false.B
      for (d <- 0 until rdata.getWidth) {
        val dPos = d + k + 1
        if ((dPos & pos) != 0) { parity = parity ^ receivedData(d) }
      }
      recomputedCheck(i) := parity
    }
    val syndrome = Wire(Vec(k, Bool()))
    for (i <- 0 until k) { syndrome(i) := receivedCheck(i) =/= recomputedCheck(i) }
    val syndromeNonZero = syndrome.asUInt =/= 0.U
    val dataXor   = (0 until rdata.getWidth).foldLeft(false.B)((p, i) => p ^ receivedData(i))
    val checkXor  = (0 until k).foldLeft(dataXor)((p, i) => p ^ recomputedCheck(i))
    val parityMismatch = receivedParity =/= checkXor
    val syndromeIdx   = syndrome.asUInt - 1.U
    val correctedData = Wire(UInt(rdata.getWidth.W))
    correctedData := receivedData
    when(syndromeNonZero && parityMismatch) { correctedData := receivedData ^ (1.U << syndromeIdx) }
    val err  = syndromeNonZero
    val uerr = syndromeNonZero && !parityMismatch
    (correctedData, err, uerr)
  }

  def decodeAndCheck(rdata: UInt, dataBits: Int, protect: String, eccSegNum: Int, eccSegWidth: Int, lastEccSegWidth: Int): (UInt, Bool, Bool) = {
    protect match {
      case "none"   => (rdata, false.B, false.B)
      case "Parity" => decodeParity(rdata, dataBits, eccSegNum, eccSegWidth, lastEccSegWidth)
      case "ECC"    => decodeEccMultiSeg(rdata, dataBits, eccSegNum, eccSegWidth, lastEccSegWidth)
      case _        => (rdata, false.B, false.B)
    }
  }
}