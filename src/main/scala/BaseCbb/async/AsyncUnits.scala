package BaseCbb.async

import chisel3._
import chisel3.util._
import BaseCbb.utils.AsyncResetSynchronizerShiftReg

// 1. 两位同步器 (2-flop Synchronizer)
// 用于将单bit信号从快时钟域同步到慢时钟域，降低亚稳态概率
// 内部复用 utils 的 CDC 主原语（desiredName 编码复位类型/深度，可被后端识别替换），
// 避免两套同步链实现漂移。
class Sync2(depth: Int = 2) extends Module {
  require(depth >= 2, "synchronizer depth must be >= 2")
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val rst_n = Input(AsyncReset())
    val din   = Input(Bool())
    val dout  = Output(Bool())
  })

  withClockAndReset(io.clk, io.rst_n) {
    io.dout := AsyncResetSynchronizerShiftReg(io.din, depth, 0)
  }
}

// 2. 电平脉冲同步器 (Level-to-Pulse Synchronizer)
// 将源时钟域的脉冲同步到目标时钟域，并输出一个脉冲
class PulseSync extends Module {
  val io = IO(new Bundle {
    val srcClk   = Input(Clock())
    val srcRst_n = Input(AsyncReset())
    val dstClk   = Input(Clock())
    val dstRst_n = Input(AsyncReset())
    val pulseIn  = Input(Bool())
    val pulseOut = Output(Bool())
  })

  // 源时钟域：检测脉冲，翻转标志
  val toggleOut = Wire(Bool())
  withClockAndReset(io.srcClk, io.srcRst_n) {
    val toggle = RegInit(false.B)
    when (io.pulseIn) {
      toggle := ~toggle
    }
    // 输出到目标域
    toggleOut := toggle
  }

  // 目标时钟域：两级同步，检测边沿产生脉冲
  val toggleSync = Module(new Sync2(2))
  toggleSync.io.clk := io.dstClk
  toggleSync.io.rst_n := io.dstRst_n
  toggleSync.io.din := toggleOut

  val syncPulse = RegNext(toggleSync.io.dout, false.B)
  io.pulseOut := toggleSync.io.dout ^ syncPulse
}

// 3. 双边沿检测 (Double Edge Detector)
class EdgeDetect extends Module {
  val io = IO(new Bundle {
    val din    = Input(Bool())
    val rising = Output(Bool())
    val falling = Output(Bool())
    val any  = Output(Bool())
  })
  val delay = RegNext(io.din, false.B)
  io.rising  := io.din & !delay
  io.falling := !io.din & delay
  io.any := io.rising | io.falling
}

// 4. 经典四位全握手 (4-phase Handshake)
class Handshake[T <: Data](dataType: T) extends Module {
  val io = IO(new Bundle {
    val srcValid = Input(Bool())
    val srcReady = Output(Bool())
    val srcData  = Input(dataType)
    val dstValid = Output(Bool())
    val dstReady = Input(Bool())
    val dstData  = Output(dataType)
  })

  val regReq = RegInit(false.B)
  val regAck = RegInit(false.B)
  val regData = Reg(dataType)

  when (!regReq && io.srcValid) {
    regReq := true.B
    regData := io.srcData
  } .elsewhen (regReq && regAck) {
    regReq := false.B
  }

  when (!regAck && regReq) {
    regAck := true.B
  } .elsewhen (regAck && !regReq) {
    regAck := false.B
  }

  io.srcReady := !regReq
  io.dstValid := regReq
  io.dstData  := regData
}

// 5. 格雷码计数器 (Gray Code Counter)
class GrayCounter(width: Int = 4) extends Module {
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val rst_n = Input(AsyncReset())
    val en    = Input(Bool())
    val binary = Output(UInt(width.W))
    val gray   = Output(UInt(width.W))
  })

  withClockAndReset(io.clk, io.rst_n) {
    val binCnt = RegInit(0.U(width.W))
    when (io.en) {
      binCnt := binCnt + 1.U
    }
    io.binary := binCnt
    io.gray := (binCnt) ^ (binCnt >> 1)
  }
}
