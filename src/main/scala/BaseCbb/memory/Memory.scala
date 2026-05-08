package BaseCbb.memory
import BaseCbb.MemoryProtectType.{ECC, MemoryProtectType, Parity, ProtNone}
import chisel3._
import chisel3.experimental._
import chisel3.util.{Cat, Enum, Pipe, RegEnable, ShiftRegister, is, log2Ceil, switch}
import BaseCbb.{GenBundle, Memory}
import chisel3.stage.ChiselStage

import java.io.PrintWriter


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

class TpMemoryLgcPort(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val we     = Input(Bool())
  val re     = Input(Bool())
  val waddr  = Input(UInt(addrWidth.W))
  val raddr  = Input(UInt(addrWidth.W))
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
  def MEM_TYPE = "SIMULATION"
  //def MEM_TYPE = "BB"
}

/**
 * SpMemoryWrap — 单口 SRAM 封装，支持输入/输出插拍
 *
 * @param mem          Memory 配置对象（name/depth/dataType/flopIn/flopOut）
 * @param flopInDepth  输入侧插拍深度（-1 = 从 mem.flopIn 推导：1 或 0）
 * @param flopOutDepth 输出侧插拍深度（-1 = 从 mem.flopOut 推导：1 或 0）
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
  // we / re: RegNext × inDepth（组合旁路，仅延迟对齐）
  private val pipeInWe = (0 until inDepth).foldLeft(lgc.we)((prev, _) => RegNext(prev))
  private val pipeInRe = (0 until inDepth).foldLeft(lgc.re)((prev, _) => RegNext(prev))

  // addr / wdata: RegEnable(_, we) × inDepth（仅在 we=1 时采样）
  private val pipeInAddr  = (0 until inDepth).foldLeft(lgc.addr)((prev, _) =>
    RegEnable(prev, lgc.we||lgc.re))
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
    withClockAndReset(clk,rst_n) {
      // Output pipeline
      if (mem.flopOut) {
        lgc.rdata := RegNext(mem_inst.io.rdata)
      } else {
        lgc.rdata := mem_inst.io.rdata
      }
    }

  } else {
    withClockAndReset(clk, rst_n) {
      val mem_inst = Module(new SimMemory(mem.dataWidth,mem.depth))
      mem_inst.io.we    := pipeInWe
      mem_inst.io.re    := pipeInRe
      mem_inst.io.waddr := pipeInAddr
      mem_inst.io.raddr := pipeInAddr   // read addr follows write pipeline
      mem_inst.io.wdata := pipeInWdata

      // Output pipeline
      if(mem.flopOut){
        lgc.rdata := RegNext(mem_inst.io.rdata)
      }else{
        lgc.rdata := mem_inst.io.rdata
      }
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
    withClockAndReset(clk, rst_n) {
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

  def decodeParity(rdata: UInt,  parSegNum: Int, parSegWidth: Int, lastParSegWidth: Int): (UInt, Bool, Bool) = {
    var anyErr = false.B
    val segs = (0 until parSegNum).map { i =>
      val segBits = if (i < parSegNum - 1) parSegWidth else lastParSegWidth
      val offset  = i * (parSegWidth+1)
      val seg     = rdata(offset + segBits - 1, offset)
      val par     = rdata(offset + segBits)
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
    Cat(overallParity, checkBits.asUInt, data)
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

  def decodeAndCheck(rdata: UInt, dataBits: Int, protect: MemoryProtectType, eccSegNum: Int, eccSegWidth: Int, lastEccSegWidth: Int): (UInt, Bool, Bool) = {
    protect match {
      case ProtNone => (rdata, false.B, false.B)
      case Parity => decodeParity(rdata, eccSegNum, eccSegWidth, lastEccSegWidth)
      case ECC    => decodeEccMultiSeg(rdata, dataBits, eccSegNum, eccSegWidth, lastEccSegWidth)
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
  private val accCntW         = log2Ceil(mem.latency + 1)

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

  // ── Init FSM ─────────────────────────────────────────────────────
  val sIdle :: sInit :: Nil = Enum(2)
  val state = RegInit(sIdle)
  val initCnt = RegInit(0.U(mem.addrWidth.W))
  val initDoneReg = RegInit(false.B)

  val initActive = state === sInit
  val initWe     = state === sInit
  val initAddr   = initCnt
  val initWdata  = 0.U(mem.dataWidth.W)

  when(state === sIdle) {
    when(io.dfx.init) {
      initDoneReg := false.B
      state := sInit
      initCnt := 0.U
    }
  }.elsewhen(state === sInit) {
    when(initCnt === (mem.depth - 1).U) {
      state := sIdle
      initDoneReg := true.B
    }.otherwise {
      initCnt := initCnt + 1.U
    }
  }

  io.dfx.initDone := initDoneReg

  // ── ECC decode (continuous; must precede CPU FSM for forward ref) ──
  private val rawRdata = memWrap.lgc.rdata
  val rdataReg = rawRdata

  val (decData, err, uerr) = EccCodec.decodeAndCheck(
    rdataReg, dataBits, mem.protect, eccSegNum, eccSegWidth, lastEccSegWidth
  )

  // ── CPU access logic ─────────────────────────────────────────────
  val (cpuMemStart, cpuBlockUser, cpuWdataRaw, cpuWe, cpuRe, cpuAddr) = if (mem.RsAccess) {

    // Separate idle check: CPU read waits only for user read, CPU write waits only for user write
    val userReadActive  = reFlopped || initActive
    val userWriteActive = weFlopped || initActive
    val cpuBlockedByUser = Mux(io.cpu.re, userReadActive,
                              Mux(io.cpu.we, userWriteActive, false.B))

    // CPU access FSM
    val sCpuIdle :: sCpuWait :: sCpuAccess :: sCpuDone :: Nil = Enum(4)
    val cpuState     = RegInit(sCpuIdle)
    val cpuWaitCnt   = RegInit(0.U(16.W))
    val cpuAccessCnt = RegInit(0.U(accCntW.W))
    val cpuRdataReg  = RegInit(0.U(dataBits.W))
    val cpuAckReg    = RegInit(false.B)
    val cpuStatusReg = RegInit(0.U(2.W))

    val cpuReq = io.cpu.re || io.cpu.we

    // cpuMemStart: combinational, fires in same cycle idle is detected (one idle cycle)
    val memStart = (cpuState === sCpuIdle && cpuReq && !cpuBlockedByUser) ||
                   (cpuState === sCpuWait && !cpuBlockedByUser)

    io.cpuBackpress := cpuState === sCpuWait && cpuWaitCnt >= io.cpuCfg.idleCycleTh0

    cpuAckReg := false.B  // default: single-cycle pulse

    switch(cpuState) {
      is(sCpuIdle) {
        when(cpuReq) {
          when(!cpuBlockedByUser) {
            cpuState     := sCpuAccess
            cpuAccessCnt := 1.U
          }.otherwise {
            cpuState   := sCpuWait
            cpuWaitCnt := 0.U
          }
        }
      }

      is(sCpuWait) {
        cpuWaitCnt := cpuWaitCnt + 1.U
        when(cpuWaitCnt >= mem.RsMemoryDisLat.U) {
          // Timeout: ACK=1, rdata=all-1s, status=3
          cpuState     := sCpuDone
          cpuRdataReg  := ~0.U(dataBits.W)
          cpuStatusReg := 3.U
          cpuAckReg    := true.B
        }.elsewhen(!cpuBlockedByUser) {
          cpuState     := sCpuAccess
          cpuAccessCnt := 1.U
        }
      }

      is(sCpuAccess) {
        cpuAccessCnt := cpuAccessCnt + 1.U
        when(cpuAccessCnt === mem.latency.U) {
          cpuState     := sCpuDone
          cpuRdataReg  := decData
          cpuStatusReg := Mux(io.cpu.re && uerr, 1.U, 0.U)
          cpuAckReg    := true.B
        }
      }

      is(sCpuDone) {
        when(!cpuReq) {
          cpuState := sCpuIdle
        }
      }
    }

    // Connect CPU outputs
    io.cpu.rdata  := cpuRdataReg
    io.cpu.ack    := cpuAckReg
    io.cpu.status := cpuStatusReg

    val blockUser = cpuState === sCpuAccess || cpuState === sCpuDone
    val wdataRaw  = io.cpu.wdata
    val cpu_we    = io.cpu.we
    val cpu_re    = io.cpu.re
    val cpu_addr  = io.cpu.addr

    (memStart, blockUser, wdataRaw, cpu_we, cpu_re, cpu_addr)
  } else {
    // Tie off CPU outputs when RsAccess disabled
    io.cpu.rdata     := 0.U
    io.cpu.ack       := false.B
    io.cpu.status    := 0.U
    io.cpuBackpress  := false.B
    (false.B, false.B, 0.U(dataBits.W), false.B, false.B, 0.U(mem.addrWidth.W))
  }

  // ── Shared ECC/Parity encoding ───────────────────────────────────
  private val wdataPreEncode = Mux(initActive, initWdata,
                                 Mux(cpuMemStart, cpuWdataRaw, wdataFlopped))
  private val encodedWdata = mem.protect match {
    case ProtNone => wdataPreEncode
    case Parity   => EccCodec.encodeParity(wdataPreEncode, eccSegNum, eccSegWidth, lastEccSegWidth)
    case ECC      => EccCodec.encodeEcc(wdataPreEncode, eccSegNum, eccSegWidth, lastEccSegWidth)
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
  private val accCntW         = log2Ceil(mem.latency + 1)

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

  // ── Init FSM ─────────────────────────────────────────────────────
  private val sIdle :: sInit :: Nil = Enum(2)
  private val state       = RegInit(sIdle)
  private val initCnt     = RegInit(0.U(mem.addrWidth.W))
  private val initDoneReg = RegInit(false.B)

  private val initActive = state === sInit
  private val initWe     = state === sInit
  private val initAddr   = initCnt
  private val initWdata  = 0.U(mem.dataWidth.W)

  when(state === sIdle) {
    when(io.dfx.init) {
      initDoneReg := false.B
      state   := sInit
      initCnt := 0.U
    }
  }.elsewhen(state === sInit) {
    when(initCnt === (mem.depth - 1).U) {
      state       := sIdle
      initDoneReg := true.B
    }.otherwise {
      initCnt := initCnt + 1.U
    }
  }

  io.dfx.initDone := initDoneReg

  // ── ECC decode (continuous; must precede CPU FSM for forward ref) ──
  private val rawRdata = memWrap.lgc.rdata
  val rdataReg = rawRdata

  val (decData, err, uerr) = EccCodec.decodeAndCheck(
    rdataReg, dataBits, mem.protect, eccSegNum, eccSegWidth, lastEccSegWidth
  )

  // ── CPU access logic ─────────────────────────────────────────────
  val (cpuMemStart, cpuBlockUser, cpuWdataRaw, cpuWe, cpuRe, cpuWaddr, cpuRaddr) = if (mem.RsAccess) {

    // Separate idle check: CPU read waits only for user read, CPU write waits only for user write
    val userReadActive  = reFlopped || initActive
    val userWriteActive = weFlopped || initActive
    val cpuBlockedByUser = Mux(io.cpu.re, userReadActive,
                              Mux(io.cpu.we, userWriteActive, false.B))

    val sCpuIdle :: sCpuWait :: sCpuAccess :: sCpuDone :: Nil = Enum(4)
    val cpuState     = RegInit(sCpuIdle)
    val cpuWaitCnt   = RegInit(0.U(16.W))
    val cpuAccessCnt = RegInit(0.U(accCntW.W))
    val cpuRdataReg  = RegInit(0.U(dataBits.W))
    val cpuAckReg    = RegInit(false.B)
    val cpuStatusReg = RegInit(0.U(2.W))

    val cpuReq = io.cpu.re || io.cpu.we

    // cpuMemStart: combinational, fires in same cycle idle is detected
    val memStart = (cpuState === sCpuIdle && cpuReq && !cpuBlockedByUser) ||
                   (cpuState === sCpuWait && !cpuBlockedByUser)

    io.cpuBackpress := cpuState === sCpuWait && cpuWaitCnt >= io.cpuCfg.idleCycleTh0

    cpuAckReg := false.B  // default: single-cycle pulse

    switch(cpuState) {
      is(sCpuIdle) {
        when(cpuReq) {
          when(!cpuBlockedByUser) {
            cpuState     := sCpuAccess
            cpuAccessCnt := 1.U
          }.otherwise {
            cpuState   := sCpuWait
            cpuWaitCnt := 0.U
          }
        }
      }

      is(sCpuWait) {
        cpuWaitCnt := cpuWaitCnt + 1.U
        when(cpuWaitCnt >= mem.RsMemoryDisLat.U) {
          cpuState     := sCpuDone
          cpuRdataReg  := ~0.U(dataBits.W)
          cpuStatusReg := 3.U
          cpuAckReg    := true.B
        }.elsewhen(!cpuBlockedByUser) {
          cpuState     := sCpuAccess
          cpuAccessCnt := 1.U
        }
      }

      is(sCpuAccess) {
        cpuAccessCnt := cpuAccessCnt + 1.U
        when(cpuAccessCnt === mem.latency.U) {
          cpuState     := sCpuDone
          cpuRdataReg  := decData
          cpuStatusReg := Mux(io.cpu.re && uerr, 1.U, 0.U)
          cpuAckReg    := true.B
        }
      }

      is(sCpuDone) {
        when(!cpuReq) {
          cpuState := sCpuIdle
        }
      }
    }

    io.cpu.rdata  := cpuRdataReg
    io.cpu.ack    := cpuAckReg
    io.cpu.status := cpuStatusReg

    val blockUser = cpuState === sCpuAccess || cpuState === sCpuDone
    val wdataRaw  = io.cpu.wdata
    val cpu_we    = io.cpu.we
    val cpu_re    = io.cpu.re
    val cpu_waddr = io.cpu.addr
    val cpu_raddr = io.cpu.addr

    (memStart, blockUser, wdataRaw, cpu_we, cpu_re, cpu_waddr, cpu_raddr)
  } else {
    // Tie off CPU outputs when RsAccess disabled
    io.cpu.rdata     := 0.U
    io.cpu.ack       := false.B
    io.cpu.status    := 0.U
    io.cpuBackpress  := false.B
    (false.B, false.B, 0.U(dataBits.W), false.B, false.B, 0.U(mem.addrWidth.W), 0.U(mem.addrWidth.W))
  }

  // ── Shared ECC/Parity encoding ───────────────────────────────────
  private val wdataPreEncode = Mux(initActive, initWdata,
                                 Mux(cpuMemStart, cpuWdataRaw, wdataFlopped))
  private val encodedWdata = mem.protect match {
    case ProtNone => wdataPreEncode
    case Parity   => EccCodec.encodeParity(wdataPreEncode, eccSegNum, eccSegWidth, lastEccSegWidth)
    case ECC      => EccCodec.encodeEcc(wdataPreEncode, eccSegNum, eccSegWidth, lastEccSegWidth)
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
        name    = "TestMemParity",
        dataType = UInt(64.W),
        depth   = 32,
        protect = ECC,
        CheckIn = true,
        CheckOut = true,
        RsAccess = true
      )))
    val file = s"$dir/SpMemoryWrap3.sv"
    new PrintWriter(file) { write(verilog); close() }
    println(s"Saved to $file (${verilog.length} chars)")
  }
}
