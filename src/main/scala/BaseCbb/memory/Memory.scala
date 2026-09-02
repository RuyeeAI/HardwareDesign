package BaseCbb.memory

import BaseCbb.data.GenBundle
import chisel3.util._
import chisel3._
import chisel3.stage.ChiselStage
import java.io.PrintWriter


object MemoryAccessType extends Enumeration {
  type MemoryAccessType = Value
  val SP, TP, DP, TCAM  = Value
}

object MemoryProtectType extends Enumeration{
  type MemoryProtectType = Value
  val ECC,Parity,ProtNone= Value
}
object MemoryInitType extends Enumeration{
  type MemoryInitType = Value
  val AllZero,AllOne,Incr= Value
}

// Bring only the type aliases into scope (not Value, which would be ambiguous)
import MemoryAccessType.MemoryAccessType
import MemoryInitType.MemoryInitType
import MemoryProtectType.MemoryProtectType


case class Memory(
                   /**
                    * Logic Part
                    */
                   name:String,
                   dataType:Data,
                   depth:Int,
                   memoryType:MemoryAccessType = MemoryAccessType.SP,
                   instNum:Int = 1,
                   Hazard:Boolean = false,
                   Fatal:Boolean = false,
                   RsAccess:Boolean = false,
                   initValue:MemoryInitType = MemoryInitType.AllZero,

                   /**
                    * flop in/out
                    */
                   flopIn:Boolean=false,
                   flopOut:Boolean=true,
                   CheckIn:Boolean=false,
                   CheckOut:Boolean=true,
                   protect:MemoryProtectType = MemoryProtectType.ECC,
                   isPhysicalMemory:Boolean=false,
                   /**
                    * Default configuration
                    */
                   protectWidthTh:Int = 320,
                   bypassOnConflict:Boolean = false,
                   RsMemoryDisLat:Int = 32
                 ) {
  // ── 配置合法性前置校验（避免 elaborate 到一半才报裸 require）──────────────
  require(depth > 0, s"Memory '$name': depth must be positive, got $depth")
  require(instNum > 0, s"Memory '$name': instNum must be positive, got $instNum")
  require(dataType.getWidth > 0, s"Memory '$name': dataType width must be positive")
  require(protectWidthTh >= 4,
    s"Memory '$name': protectWidthTh=$protectWidthTh too small, ECC/Parity segment requires >= 4 bits")

  def dataWidth:Int = {
    val eccSegNum = math.ceil(dataType.getWidth.toDouble / protectWidthTh).toInt
    if(protect == MemoryProtectType.ECC) {
      val eccSegWidth = math.ceil(dataType.getWidth / eccSegNum).toInt
      val lastEccSegWidth = dataType.getWidth - (eccSegNum - 1) * eccSegWidth
      val eccTotalWidth = (eccWidth(eccSegWidth) + 1) * (eccSegNum - 1) + (eccWidth(lastEccSegWidth) + 1)
      eccTotalWidth + dataType.getWidth
    }else if(protect == MemoryProtectType.Parity){
      dataType.getWidth + eccSegNum
    }else{
      dataType.getWidth
    }
  }

  def lastCheckSegWidth:Int = {
    val eccSegNum = math.ceil(dataType.getWidth.toDouble / protectWidthTh).toInt
    if(protect == MemoryProtectType.ECC | protect == MemoryProtectType.Parity) {
      val eccSegWidth = math.ceil(dataType.getWidth / eccSegNum).toInt
      dataType.getWidth - (eccSegNum - 1) * eccSegWidth
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

  /** SECDED 校验位宽公式单点实现（委托 EccCodec，公式详见 eccWidthOf） */
  def eccWidth(n:Int):Int = EccCodec.eccWidthOf(n)

  def addrWidth:Int = log2Ceil(depth)

  def toMap: Map[String, Any] = Map(
    "Name"       -> name,
    "AccessType" -> memoryType,
    "Width"      -> dataWidth,
    "Depth"      -> depth,
    "InstNum"    -> instNum
  )
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
 * SpMemoryLgcPort — 单口 SRAM 逻辑端口（= SpMemoryPort + ECC 不可纠正错误上报）
 * 合并自 SpMemoryPort：仅追加 uecErr 字段（基类统一为 GenBundle）
 */
class SpMemoryLgcPort(addrWidth: Int, dataWidth: Int) extends SpMemoryPort(addrWidth, dataWidth) {
  val uecErr = Output(Bool())
}

/**
 * TpMemoryLgcPort — 双口 SRAM 逻辑端口（= TpMemoryPort + ECC 不可纠正错误上报）
 */
class TpMemoryLgcPort(addrWidth: Int, dataWidth: Int) extends TpMemoryPort(addrWidth, dataWidth) {
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
  // Error injection
  val injCorrEn  = Input(Bool())                // 注入可纠正错误的使能
  val injUerrEn  = Input(Bool())                // 注入不可纠正错误的使能
  val injDone    = Output(Bool())               // 注入操作完成
}

/** CpuRsPort — CPU 访问 SRAM 的端口，与 MemoryRsPort 一致 */
class CpuRsPort(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val re     = Input(Bool())
  val we     = Input(Bool())
  val addr   = Input(UInt(addrWidth.W))
  val wdata  = Input(UInt(dataWidth.W))
  val rdata  = Output(UInt(dataWidth.W))
  val ack    = Output(Bool())
  val status = Output(UInt(2.W))
}

class SpMemoryBB(mem:Memory) extends BlackBox{
  override def desiredName=mem.name+"_SP_BB_"+mem.depth+"X_"+mem.dataWidth
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
  override def desiredName=mem.name+"_TP_BB_"+mem.depth+"X_"+mem.dataWidth
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

class SimMemory(dataWidth:Int,depth:Int) extends Module {
  var addrWidth = log2Ceil(depth)
  val io = IO(new TpMemoryPort(addrWidth, dataWidth))

  val m     = Reg(Vec(depth, UInt(dataWidth.W))).suggestName("MemStorage")
  val we    = Wire(Bool())
  val wdata = Wire(UInt(dataWidth.W))
  val waddr = Wire(UInt(addrWidth.W))
  val raddr = Wire(UInt(addrWidth.W))
  val re    = Wire(Bool())
  we    := io.we
  re    := io.re
  waddr := io.waddr
  wdata := io.wdata
  raddr := io.raddr
  when(we) {
    m(waddr) := wdata
  }
  io.rdata := RegNext(m(raddr))
}

class MemoryWrap extends RawModule{
  //Change the memory to Simulation or Physical memory
}

/**
 * SpMemoryWrap — 单口 SRAM 封装，支持输入/输出插拍
 *
 * @param mem          Memory 配置对象（name/depth/dataType/flopIn/flopOut）
 */
class SpMemoryWrap(
  mem:          Memory
) extends MemoryWrap {

  private val inDepth  = if (mem.flopIn)  1 else 0
  private val outDepth = if (mem.flopOut) 1 else 0

  require(inDepth  >= 0)
  require(outDepth >= 0)

  val clk   = IO(Input(Clock()))
  val rst_n = IO(Input(Bool()))
  val lgc   = IO(new SpMemoryPort(mem.addrWidth, mem.dataWidth))

  // ================================================================
  // Input pipeline chain
  // ================================================================
  withClockAndReset(clk, !rst_n) {
    // --------------------------------------------------------------
    // Input pipeline chain
    // --------------------------------------------------------------
    // we / re: RegNext × inDepth（组合旁路，仅延迟对齐）
    val pipeInWe = (0 until inDepth).foldLeft(lgc.we)((prev, _) => RegNext(prev))
    val pipeInRe = (0 until inDepth).foldLeft(lgc.re)((prev, _) => RegNext(prev))

    // addr / wdata: RegEnable(_, we) × inDepth（仅在 we=1 时采样）
    val pipeInAddr  = (0 until inDepth).foldLeft(lgc.addr)((prev, _) =>
      RegEnable(prev, lgc.we || lgc.re))
    val pipeInWdata = (0 until inDepth).foldLeft(lgc.wdata)((prev, _) =>
      RegEnable(prev, lgc.we))

    // --------------------------------------------------------------
    // Physical memory instance
    // --------------------------------------------------------------
    if (mem.isPhysicalMemory) {
      val mem_inst = Module(new SpMemoryBB(mem)).suggestName(mem.name + "_PHY_MEM")
      mem_inst.io.clk  := clk
      mem_inst.io.we    := pipeInWe
      mem_inst.io.re    := pipeInRe
      mem_inst.io.addr  := pipeInAddr
      mem_inst.io.wdata := pipeInWdata
      // Output pipeline（outDepth 统一控制，与 TpMemoryWrap 行为一致）
      lgc.rdata := (0 until outDepth).foldLeft(mem_inst.io.rdata)((prev, _) => RegNext(prev))

    } else {
      val mem_inst = Module(new SimMemory(mem.dataWidth,mem.depth))
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
  val lgc   = IO(new TpMemoryPort(mem.addrWidth, mem.dataWidth))

  // ================================================================
  // 同 SpMemoryWrap：rst_n 低有效显式转高有效；RawModule 全部时序逻辑
  //（含 flopIn=true 时的输入流水）必须在同一时钟域内。
  // ================================================================
  withClockAndReset(clk, !rst_n) {
    // --------------------------------------------------------------
    // Input pipeline chain
    // --------------------------------------------------------------
    // we / re: RegNext × inDepth
    val pipeInWe = (0 until inDepth).foldLeft(lgc.we)((prev, _) => RegNext(prev))
    val pipeInRe = (0 until inDepth).foldLeft(lgc.re)((prev, _) => RegNext(prev))

    // addr / wdata: RegEnable(_, we) × inDepth
    val pipeInWaddr = (0 until inDepth).foldLeft(lgc.waddr)((prev, _) =>
      RegEnable(prev, lgc.we))
    val pipeInRaddr = (0 until inDepth).foldLeft(lgc.raddr)((prev, _) =>
      RegEnable(prev, lgc.re))
    val pipeInWdata = (0 until inDepth).foldLeft(lgc.wdata)((prev, _) =>
      RegEnable(prev, lgc.we))

    // --------------------------------------------------------------
    // Physical memory instance
    // --------------------------------------------------------------
    if (mem.isPhysicalMemory) {
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
      val mem_inst = Module(new SimMemory(mem.dataWidth,mem.depth))
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
    if ((1 << k) >= (segBits + k + 1)) k else k + 1
  }

  /**
   * 标准汉明位置表：数据位依次占据非 2 的幂位置（1-based），校验位 i 位于 2^i。
   * 这样 syndrome 直接给出出错位置，且数据位/校验位位置不会碰撞
   * （旧实现 dPos = d + k + 1 会与 2^i 重叠，且纠错翻转位偏了 k）。
   */
  private[memory] def hammingDataPositions(segBits: Int, k: Int): Seq[Int] = {
    val out = scala.collection.mutable.ArrayBuffer[Int]()
    var p = 1
    while (out.size < segBits) {
      if ((p & (p - 1)) != 0) out += p
      p += 1
    }
    require(out.last < (1 << k),
      s"Hamming positions (max=${out.last}) exceed k=$k check bits for segBits=$segBits")
    out.toSeq
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

  /**
   * 奇偶校验解码。布局与 encodeParity 严格镜像：
   *   encodeParity = Cat(parityBits.reverse, data) —— 数据连续在低位，校验位连续在高位，
   *   其中段 i 的校验位位于 bit (dataBits + (parSegNum-1-i))。
   * （旧实现按"每段后跟 1 bit 校验"的交错布局取位，多段时与编码端不一致。）
   */
  def decodeParity(rdata: UInt, parSegNum: Int, parSegWidth: Int, lastParSegWidth: Int): (UInt, Bool, Bool) = {
    val dataBits = rdata.getWidth - parSegNum
    var anyErr = false.B
    val segs = (0 until parSegNum).map { i =>
      val segBits = if (i < parSegNum - 1) parSegWidth else lastParSegWidth
      val offset  = i * parSegWidth
      val seg     = rdata(offset + segBits - 1, offset)
      val par     = rdata(dataBits + (parSegNum - 1 - i))
      val calc    = seg.asUInt.xorR
      anyErr      = anyErr || (par =/= calc)
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
    val W         = data.getWidth
    val positions = hammingDataPositions(W, k)
    val checkBits = Wire(Vec(k, Bool()))
    for (i <- 0 until k) {
      val pos = 1 << i
      var parity = false.B
      for (d <- 0 until W) {
        if ((positions(d) & pos) != 0) { parity = parity ^ data(d) }
      }
      checkBits(i) := parity
    }
    val dataXor       = (0 until W).foldLeft(false.B)((p, i) => p ^ data(i))
    val checkXor      = (0 until k).foldLeft(dataXor)((p, i) => p ^ checkBits(i))
    val overallParity = checkXor
    Cat(overallParity, checkBits.asUInt, data)
  }

  def decodeEccMultiSeg(rdata: UInt, dataBits: Int, eccSegNum: Int, eccSegWidth: Int, lastEccSegWidth: Int): (UInt, Bool, Bool) = {
    var anyErr    = false.B
    var anyUerr   = false.B
    // 段物理宽度与 encodeEcc 的 Cat(segEncoded.reverse) 布局严格镜像：
    // Cat 的首参数在高位，因此段 0 位于最低位、段 n-1 位于最高位，
    // 段 i 起始偏移 = 宽度比它低的各段（j<i）之和（各段宽度可能不等，不能用「段宽 × 序号」）。
    val segWidths = (0 until eccSegNum).map { i =>
      val sb = if (i < eccSegNum - 1) eccSegWidth else lastEccSegWidth
      sb + eccWidthOf(sb) + 1
    }
    val segStarts = segWidths.scanLeft(0)(_ + _)  // segStarts(i) = 段 i 之前的累计宽度
    val segDecoded = (0 until eccSegNum).map { i =>
      val segBits     = if (i < eccSegNum - 1) eccSegWidth else lastEccSegWidth
      val k           = eccWidthOf(segBits)
      val eccBitsThis = k + 1
      val segOffset   = segStarts(i)
      val segRdata = rdata(segOffset + segBits - 1, segOffset)
      val segEcc   = rdata(segOffset + segBits + eccBitsThis - 1, segOffset + segBits)
      val (decSeg, err, uerr) = decodeEccSeg(segRdata, segEcc, k)
      anyErr  = anyErr  || err
      anyUerr = anyUerr || uerr
      decSeg
    }
    (Cat(segDecoded.reverse), anyErr, anyUerr)
  }

  def decodeEccSeg(rdata: UInt, eccFull: UInt, k: Int): (UInt, Bool, Bool) = {
    val W              = rdata.getWidth
    val receivedData   = rdata
    val receivedCheck  = eccFull(k - 1, 0)
    val receivedParity = eccFull(k)
    val positions      = hammingDataPositions(W, k)
    val recomputedCheck = Wire(Vec(k, Bool()))
    for (i <- 0 until k) {
      val pos = 1 << i
      var parity = false.B
      for (d <- 0 until W) {
        if ((positions(d) & pos) != 0) { parity = parity ^ receivedData(d) }
      }
      recomputedCheck(i) := parity
    }
    val syndrome = Wire(Vec(k, Bool()))
    for (i <- 0 until k) { syndrome(i) := receivedCheck(i) =/= recomputedCheck(i) }
    val syndromeVal     = syndrome.asUInt
    val syndromeNonZero = syndromeVal =/= 0.U
    // syndrome 为 2 的幂 → 出错的是校验位自身（标准汉明下数据位永不落在 2 的幂上），
    // 数据无需纠正；旧实现此处会把校验位错误误判成数据位错并翻错位。
    val isCheckPos = syndromeNonZero && ((syndromeVal & (syndromeVal - 1.U)) === 0.U)
    // 整体校验：对"接收到的码字"（数据位 + 接收到的校验位）求奇偶，与存储的总校验位比较。
    // 必须用接收到的校验位而非重算值 —— 若用重算值，落在奇重数汉明位置（如 7=0b111）上的
    // 单比特错会因数据位与校验位翻转相互抵消而被误判为双比特错、且不纠错。
    val parityMismatch = receivedParity =/= (receivedData.xorR ^ receivedCheck.xorR)
    // 出错数据位索引：syndromeVal 命中哪个数据位位置（位置互异，至多一个命中）
    val corrIdxWidth = math.max(1, log2Ceil(W))
    val corrIdx = Wire(UInt(corrIdxWidth.W))
    corrIdx := 0.U
    for (d <- 0 until W) {
      when(syndromeVal === positions(d).U) { corrIdx := d.U }
    }
    val correctedData = Wire(UInt(W.W))
    correctedData := receivedData
    when(syndromeNonZero && parityMismatch && !isCheckPos) {
      correctedData := receivedData ^ (1.U << corrIdx).asUInt
    }
    // 分类（标准 SECDED）：
    //   syndrome≠0 且整体校验失配 → 单比特错（数据位可纠，校验位错则数据本来就对）；
    //   syndrome≠0 且整体校验匹配 → 双比特错（不可纠正）；
    //   syndrome=0 且整体校验失配 → 仅总校验位自身出错（数据无恙）。
    val err  = syndromeNonZero || parityMismatch
    val uerr = syndromeNonZero && !parityMismatch
    (correctedData, err, uerr)
  }

  def decodeAndCheck(rdata: UInt, dataBits: Int, protect: MemoryProtectType, eccSegNum: Int, eccSegWidth: Int, lastEccSegWidth: Int): (UInt, Bool, Bool) = {
    protect match {
      case MemoryProtectType.ProtNone => (rdata, false.B, false.B)
      case MemoryProtectType.Parity => decodeParity(rdata, eccSegNum, eccSegWidth, lastEccSegWidth)
      case MemoryProtectType.ECC    => decodeEccMultiSeg(rdata, dataBits, eccSegNum, eccSegWidth, lastEccSegWidth)
      case _        => (rdata, false.B, false.B)
    }
  }
}

/**
 * SpMemoryWrap3 — 单口 SRAM ECC/Parity 封装层
 *
 * 层级关系:
 *   User Logic ──► SpMemoryWrap3 ──► SpMemoryWrap ──► SpMemoryBB/SimMemory
 *
 * @param mem    Memory 配置对象
 */
class SpMemoryWrap3(mem: Memory) extends Module {

  private val dataBits        = mem.dataType.getWidth
  private val eccSegNum       = math.ceil(dataBits.toDouble / mem.protectWidthTh).toInt
  private val eccSegWidth     = math.ceil(dataBits.toDouble / eccSegNum).toInt
  private val lastEccSegWidth = dataBits - (eccSegNum - 1) * eccSegWidth

  val io = IO(new Bundle {
    val lgc         = new SpMemoryLgcPort(mem.addrWidth, dataBits)
    val dfx         = new MemoryDfxPort(mem.addrWidth)
    val cpu         = new CpuRsPort(mem.addrWidth, dataBits)
    val cpuCfg      = Input(new Bundle {
      val idleCycleTh0 = UInt(16.W)
    })
    val cpuBackpress = Output(Bool())
  })

  // ── CheckIn flops (user logic side) ──────────────────────────────
  private val wdataFlopped = if (mem.CheckIn) RegEnable(io.lgc.wdata, io.lgc.we) else io.lgc.wdata
  private val weFlopped    = if (mem.CheckIn) RegNext(io.lgc.we,    false.B) else io.lgc.we
  private val reFlopped    = if (mem.CheckIn) RegNext(io.lgc.re,    false.B) else io.lgc.re
  private val addrFlopped  = if (mem.CheckIn) RegEnable(io.lgc.addr,  io.lgc.we) else io.lgc.addr

  private val memWrap = Module(new SpMemoryWrap(mem))
  memWrap.clk   := clock
  memWrap.rst_n := !reset.asBool

  // ── ECC decode (continuous; feeds CBB CPU FSM) ──────────────────
  private val rawRdata = memWrap.lgc.rdata
  val rdataReg = rawRdata

  val (decData, err, uerr) = EccCodec.decodeAndCheck(
    rdataReg, dataBits, mem.protect, eccSegNum, eccSegWidth, lastEccSegWidth
  )

  // ── Init FSM + CPU access FSM（公共 CBB）─────────────────────────
  private val initCpu = Module(new MemInitCpuAccess(
    dataBits = dataBits, addrWidth = mem.addrWidth, latency = mem.latency,
    depth = mem.depth, RsAccess = mem.RsAccess, RsMemoryDisLat = mem.RsMemoryDisLat,
    dualPortAddr = false))
  initCpu.io.init             := io.dfx.init
  initCpu.io.idleCycleTh0     := io.cpuCfg.idleCycleTh0
  initCpu.io.cpu              <> io.cpu
  initCpu.io.userReadActive   := reFlopped || initCpu.io.initActive
  initCpu.io.userWriteActive  := weFlopped || initCpu.io.initActive
  initCpu.io.decData          := decData
  initCpu.io.uerr             := uerr

  private val initActive   = initCpu.io.initActive
  private val initWe       = initCpu.io.initWe
  private val initAddr     = initCpu.io.initAddr
  private val initWdata    = initCpu.io.initWdata
  private val cpuMemStart  = initCpu.io.cpuMemStart
  private val cpuBlockUser = initCpu.io.blockUser
  private val cpuWdataRaw  = initCpu.io.cpuWdataRaw
  private val cpuWe        = initCpu.io.cpuWe
  private val cpuRe        = initCpu.io.cpuRe
  private val cpuAddr      = initCpu.io.cpuWaddr
  io.dfx.initDone    := initCpu.io.initDone
  io.cpuBackpress    := initCpu.io.cpuBackpress

  // ── Shared ECC/Parity encoding ───────────────────────────────────
  private val wdataPreEncode = Mux(initActive, initWdata,
                                 Mux(cpuMemStart, cpuWdataRaw, wdataFlopped))
  private val encodedWdata = mem.protect match {
    case MemoryProtectType.ProtNone => wdataPreEncode
    case MemoryProtectType.Parity   => EccCodec.encodeParity(wdataPreEncode, eccSegNum, eccSegWidth, lastEccSegWidth)
    case MemoryProtectType.ECC      => EccCodec.encodeEcc(wdataPreEncode, eccSegNum, eccSegWidth, lastEccSegWidth)
    case _        => wdataPreEncode
  }

  // ── Memory input mux (priority: init > CPU-start > CPU-block > user) ──
  memWrap.lgc.we    := Mux(initActive, initWe,
                          Mux(cpuMemStart, cpuWe,
                            Mux(cpuBlockUser, false.B, weFlopped)))
  memWrap.lgc.re    := Mux(initActive, false.B,
                          Mux(cpuMemStart, cpuRe,
                            Mux(cpuBlockUser, false.B, reFlopped)))
  memWrap.lgc.addr  := Mux(initActive, initAddr,
                          Mux(cpuMemStart, cpuAddr, addrFlopped))
  memWrap.lgc.wdata := encodedWdata

  // ── ECC decode output path ───────────────────────────────────────
  private val gateReg = ShiftRegister(reFlopped, mem.latency, false.B, true.B)

  // Error injection: single-shot, captured on same cycle as re
  private val injCorrReq = io.dfx.injCorrEn && reFlopped
  private val injUerrReq = io.dfx.injUerrEn && reFlopped

  private val injCorrPipe = ShiftRegister(injCorrReq, mem.latency, false.B, true.B)
  private val injUerrPipe = ShiftRegister(injUerrReq, mem.latency, false.B, true.B)

  private val errOut  = err  || injCorrPipe
  private val uerrOut = uerr || injUerrPipe

  val rdataOutReg = if(mem.CheckOut) RegEnable(decData, gateReg) else decData
  val errOutReg   = if(mem.CheckOut) RegNext(errOut & gateReg) else errOut & gateReg
  val uerrOutReg  = if(mem.CheckOut) RegNext(uerrOut & gateReg) else uerrOut & gateReg
  val errAddrReg  = if(mem.CheckOut) RegEnable(addrFlopped, errOutReg || uerrOutReg) else addrFlopped

  // Gate user-logic outputs during CPU access
  io.lgc.rdata  := Mux(cpuBlockUser, 0.U, rdataOutReg)
  io.lgc.uecErr := Mux(cpuBlockUser, false.B, uerrOutReg)
  io.dfx.eccErr     := Mux(cpuBlockUser, false.B, errOutReg)
  io.dfx.eccUerr    := Mux(cpuBlockUser, false.B, uerrOutReg)
  io.dfx.eccErrAddr := errAddrReg
  io.dfx.injDone    := injCorrReq || injUerrReq

}

/**
 * TpMemoryWrap3 — 双口 SRAM ECC/Parity 封装层
 *
 * 层级关系:
 *   User Logic ──► TpMemoryWrap3 ──► TpMemoryWrap ──► TpMemoryBB/SimMemory
 *
 * @param mem    Memory 配置对象
 */
class TpMemoryWrap3(mem: Memory) extends Module {

  private val dataBits        = mem.dataType.getWidth
  private val eccSegNum       = math.ceil(dataBits.toDouble / mem.protectWidthTh).toInt
  private val eccSegWidth     = math.ceil(dataBits.toDouble / eccSegNum).toInt
  private val lastEccSegWidth = dataBits - (eccSegNum - 1) * eccSegWidth

  val io = IO(new Bundle {
    val lgc         = new TpMemoryLgcPort(mem.addrWidth, dataBits)
    val dfx         = new MemoryDfxPort(mem.addrWidth)
    val cpu         = new CpuRsPort(mem.addrWidth, dataBits)
    val cpuCfg      = Input(new Bundle {
      val idleCycleTh0 = UInt(16.W)
    })
    val cpuBackpress = Output(Bool())
  })

  // ── CheckIn flops (user logic side) ──────────────────────────────
  private val wdataFlopped = if (mem.CheckIn) RegEnable(io.lgc.wdata, io.lgc.we) else io.lgc.wdata
  private val weFlopped    = if (mem.CheckIn) RegNext(io.lgc.we,    false.B) else io.lgc.we
  private val reFlopped    = if (mem.CheckIn) RegNext(io.lgc.re,    false.B) else io.lgc.re
  private val waddrFlopped = if (mem.CheckIn) RegEnable(io.lgc.waddr, io.lgc.we) else io.lgc.waddr
  private val raddrFlopped = if (mem.CheckIn) RegEnable(io.lgc.raddr, io.lgc.re) else io.lgc.raddr

  private val memWrap = Module(new TpMemoryWrap(mem))
  memWrap.clk   := clock
  memWrap.rst_n := !reset.asBool

  // ── ECC decode (continuous; feeds CBB CPU FSM) ──────────────────
  private val rawRdata = memWrap.lgc.rdata
  val rdataReg = rawRdata

  val (decData, err, uerr) = EccCodec.decodeAndCheck(
    rdataReg, dataBits, mem.protect, eccSegNum, eccSegWidth, lastEccSegWidth
  )

  // ── Init FSM + CPU access FSM（公共 CBB）─────────────────────────
  private val initCpu = Module(new MemInitCpuAccess(
    dataBits = dataBits, addrWidth = mem.addrWidth, latency = mem.latency,
    depth = mem.depth, RsAccess = mem.RsAccess, RsMemoryDisLat = mem.RsMemoryDisLat,
    dualPortAddr = true))
  initCpu.io.init             := io.dfx.init
  initCpu.io.idleCycleTh0     := io.cpuCfg.idleCycleTh0
  initCpu.io.cpu              <> io.cpu
  initCpu.io.userReadActive   := reFlopped || initCpu.io.initActive
  initCpu.io.userWriteActive  := weFlopped || initCpu.io.initActive
  initCpu.io.decData          := decData
  initCpu.io.uerr             := uerr

  private val initActive   = initCpu.io.initActive
  private val initWe       = initCpu.io.initWe
  private val initAddr     = initCpu.io.initAddr
  private val initWdata    = initCpu.io.initWdata
  private val cpuMemStart  = initCpu.io.cpuMemStart
  private val cpuBlockUser = initCpu.io.blockUser
  private val cpuWdataRaw  = initCpu.io.cpuWdataRaw
  private val cpuWe        = initCpu.io.cpuWe
  private val cpuRe        = initCpu.io.cpuRe
  private val cpuWaddr     = initCpu.io.cpuWaddr
  private val cpuRaddr     = initCpu.io.cpuRaddr
  io.dfx.initDone    := initCpu.io.initDone
  io.cpuBackpress    := initCpu.io.cpuBackpress

  // ── Shared ECC/Parity encoding ───────────────────────────────────
  private val wdataPreEncode = Mux(initActive, initWdata,
                                 Mux(cpuMemStart, cpuWdataRaw, wdataFlopped))
  private val encodedWdata = mem.protect match {
    case MemoryProtectType.ProtNone => wdataPreEncode
    case MemoryProtectType.Parity   => EccCodec.encodeParity(wdataPreEncode, eccSegNum, eccSegWidth, lastEccSegWidth)
    case MemoryProtectType.ECC      => EccCodec.encodeEcc(wdataPreEncode, eccSegNum, eccSegWidth, lastEccSegWidth)
    case _        => wdataPreEncode
  }

  // ── Memory input mux (priority: init > CPU-start > CPU-block > user) ──
  memWrap.lgc.we    := Mux(initActive, initWe,
                          Mux(cpuMemStart, cpuWe,
                            Mux(cpuBlockUser, false.B, weFlopped)))
  memWrap.lgc.re    := Mux(initActive, false.B,
                          Mux(cpuMemStart, cpuRe,
                            Mux(cpuBlockUser, false.B, reFlopped)))
  memWrap.lgc.waddr := Mux(initActive, initAddr,
                          Mux(cpuMemStart, cpuWaddr, waddrFlopped))
  memWrap.lgc.raddr := Mux(cpuMemStart, cpuRaddr,
                          Mux(cpuBlockUser, 0.U, raddrFlopped))
  memWrap.lgc.wdata := encodedWdata

  // ── Bypass (disabled during CPU access) ──────────────────────────
  private val sameAddrRW = mem.bypassOnConflict.B && weFlopped && reFlopped &&
                            waddrFlopped === raddrFlopped && !cpuBlockUser && !cpuMemStart
  private val bypassData  = RegEnable(wdataFlopped, sameAddrRW)
  private val bypassValid = ShiftRegister(sameAddrRW, mem.latency, false.B, true.B)

  // ── ECC decode output path ───────────────────────────────────────
  // Uses single decoder (decData/err/uerr from above); bypass forwards raw write data
  private val gateReg = ShiftRegister(reFlopped, mem.latency, false.B, true.B)

  private val injCorrReq = io.dfx.injCorrEn && reFlopped
  private val injUerrReq = io.dfx.injUerrEn && reFlopped

  private val injCorrPipe = ShiftRegister(injCorrReq, mem.latency, false.B, true.B)
  private val injUerrPipe = ShiftRegister(injUerrReq, mem.latency, false.B, true.B)

  private val errOut  = Mux(bypassValid, false.B, err)  || injCorrPipe
  private val uerrOut = Mux(bypassValid, false.B, uerr) || injUerrPipe

  val rdataOutReg = if(mem.CheckOut) RegEnable(Mux(bypassValid, bypassData, decData), gateReg)
                    else Mux(bypassValid, bypassData, decData)
  val errOutReg   = if(mem.CheckOut) RegNext(errOut & gateReg) else errOut & gateReg
  val uerrOutReg  = if(mem.CheckOut) RegNext(uerrOut & gateReg) else uerrOut & gateReg
  val errAddrReg  = if(mem.CheckOut) RegEnable(raddrFlopped, errOutReg || uerrOutReg) else raddrFlopped

  // Gate user-logic outputs during CPU access
  io.lgc.rdata  := Mux(cpuBlockUser, 0.U, rdataOutReg)
  io.lgc.uecErr := Mux(cpuBlockUser, false.B, uerrOutReg)
  io.dfx.eccErr     := Mux(cpuBlockUser, false.B, errOutReg)
  io.dfx.eccUerr    := Mux(cpuBlockUser, false.B, uerrOutReg)
  io.dfx.eccErrAddr := errAddrReg
  io.dfx.injDone    := injCorrReq || injUerrReq

}

object EmitMemVerilog {
  def main(args: Array[String]): Unit = {

    val dir = args.headOption.getOrElse("generated")
    val verilog = ChiselStage.emitSystemVerilog(new SpMemoryWrap3(
      Memory(
        name = "SampleMem",
        dataType = UInt(32.W),
        depth = 64,
        protect = MemoryProtectType.ECC,
        flopIn = false,
        flopOut =true
      )))
    val file = s"$dir/SpMemoryWrap3.sv"
    new PrintWriter(file) { write(verilog); close() }
    println(s"Saved to $file (${verilog.length} chars)")
  }
}
