package cbb.async

import chisel3._
import chisel3.util._

// 1. 两位同步器 (2-flop Synchronizer)
// 用于将单bit信号从快时钟域同步到慢时钟域，降低亚稳态概率
class Sync2(depth: Int = 2) extends Module {
  require(depth >= 2, "synchronizer depth must be >= 2")
  val io = IO(new Bundle {
    val clk   = Input(Clock())   // 目标时钟域时钟
    val rst_n = Input(AsyncReset())
    val din   = Input(Bool())    // 异步输入信号
    val dout  = Output(Bool())   // 同步输出信号
  })

  withClockAndReset(io.clk, io.rst_n) {
    val syncStages = Reg(Vec(depth, Bool()))
    syncStages(0) := io.din
    for (i <- 1 until depth) {
      syncStages(i) := syncStages(i-1)
    }
    io.dout := syncStages(depth-1)
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
  withClockAndReset(io.srcClk, io.srcRst_n) {
    val toggle = RegInit(false.B)
    when (io.pulseIn) {
      toggle := ~toggle
    }
    val toggleSyncStage = toggle
    // 输出到目标域
    toggleSyncStage <> Wire(Bool())
    toggleSyncStage := toggle
  }

  // 目标时钟域：两级同步，检测边沿产生脉冲
  val toggleSync = Module(new Sync2(2))
  toggleSync.io.clk := io.dstClk
  toggleSync.io.rst_n := io.dstRst_n
  toggleSync.io.din := toggleSyncStage

  val syncPulse = RegNext(toggleSync.io.dout, false.B)
  io.pulseOut := toggleSync.io.dout ^ syncPulse // 检测边沿产生脉冲
}

// 3. 双边沿检测 (Double Edge Detector)
// 检测输入信号的上升沿和下降沿，输出脉冲
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

// 4. 异步复位同步释放 (Asynchronous Reset Synchronous Release)
// 将外部异步复位同步到本地时钟域，保证复位释放不出现问题
class AsyncRstSync extends Module {
  val io = IO(new Bundle {
    val clk      = Input(Clock())
    val asyncRst = Input(AsyncReset())
    val syncRst  = Output(AsyncReset())
  })

  withClockAndReset(io.clk, io.asyncRst) {
    val sync1 = RegInit(false.B)
    val sync2 = RegInit(false.B)
    sync1 := true.B
    sync2 := sync1
    io.syncRst := (!sync2).asAsyncReset
  }
}

// 5. 经典四位全握手 (4-phase Handshake)
// 源端 -> 目的端 单次传输握手
class Handshake[T <: Data](dataType: T) extends Module {
  val io = IO(new Bundle {
    // Source side
    val srcValid = Input(Bool())
    val srcReady = Output(Bool())
    val srcData  = Input(dataType)
    // Destination side
    val dstValid = Output(Bool())
    val dstReady = Input(Bool())
    val dstData  = Output(dataType)
  })

  val regReq = RegInit(false.B)
  val regAck = RegInit(false.B)
  val regData = Reg(dataType)

  // 4-phase handshake:
  // 1. src raises req (valid) when data ready
  // 2. dst accepts, raises ack (ready)
  // 3. src lowers req when ack seen
  // 4. dst lowers ack after req lowered

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

// 6. 异步FIFO (Asynchronous FIFO)
// 跨时钟域数据传输，经典的格雷码指针实现
class AsyncFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module {
  val depth = 1 << addrWidth
  val io = IO(new Bundle {
    // Write clock domain
    val wrClk   = Input(Clock())
    val wrRst_n = Input(AsyncReset())
    val wrEn    = Input(Bool())
    val din     = Input(UInt(dataWidth.W))
    val full    = Output(Bool())
    val wrLevel = Output(UInt(addrWidth.W))

    // Read clock domain
    val rdClk   = Input(Clock())
    val rdRst_n = Input(AsyncReset())
    val rdEn    = Input(Bool())
    val dout    = Output(UInt(dataWidth.W))
    val empty   = Output(Bool())
    val rdLevel = Output(UInt(addrWidth.W))
  })

  // Memory array
  val mem = Mem(depth, UInt(dataWidth.W))

  // ---------------------------
  // Write domain logic
  // ---------------------------
  withClockAndReset(io.wrClk, io.wrRst_n) {
    val wrPtrBinary = RegInit(0.U((addrWidth+1).W))
    val wrPtrGray = RegInit(0.U((addrWidth+1).W))

    when (io.wrEn && !io.full) {
      wrPtrBinary := wrPtrBinary + 1.U
      wrPtrGray := (wrPtrBinary + 1.U) ^ ((wrPtrBinary + 1.U) >> 1)
      mem.write(wrPtrBinary(addrWidth-1, 0), io.din)
    }

    // Sync read pointer to write clock domain
    val rdPtrGraySync1 = Reg(UInt((addrWidth+1).W))
    val rdPtrGraySync2 = Reg(UInt((addrWidth+1).W))
    rdPtrGraySync1 := rdPtrGray
    rdPtrGraySync2 := rdPtrGraySync1

    // Convert gray back to binary for comparison
    def grayToBinary(gray: UInt, width: Int): UInt = {
      val binary = Wire(Vec(width, UInt(1.W)))
      binary(width-1) = gray(width-1)
      for (i <- (width-2) to 0 by -1) {
        binary(i) = gray(i) ^ binary(i+1)
      }
      binary.asUInt
    }

    val rdPtrBinarySync = grayToBinary(rdPtrGraySync2, addrWidth+1)
    val wrPtrBinaryLocal = wrPtrBinary

    // Full condition: MSB different + next addr bits all same
    io.full := (wrPtrGray(addrWidth) =/= rdPtrGraySync2(addrWidth)) &&
               (wrPtrGray(addrWidth-1) =/= rdPtrGraySync2(addrWidth-1)) &&
               (wrPtrGray(addrWidth-2, 0) === rdPtrGraySync2(addrWidth-2, 0))

    io.wrLevel := (wrPtrBinaryLocal - rdPtrBinarySync)(addrWidth-1, 0)
  }

  // ---------------------------
  // Read domain logic
  // ---------------------------
  withClockAndReset(io.rdClk, io.rdRst_n) {
    val rdPtrBinary = RegInit(0.U((addrWidth+1).W))
    val rdPtrGray = RegInit(0.U((addrWidth+1).W))

    when (io.rdEn && !io.empty) {
      rdPtrBinary := rdPtrBinary + 1.U
      rdPtrGray := (rdPtrBinary + 1.U) ^ ((rdPtrBinary + 1.U) >> 1)
    }
    io.dout := mem.read(rdPtrBinary(addrWidth-1, 0))

    // Sync write pointer to read clock domain
    val wrPtrGraySync1 = Reg(UInt((addrWidth+1).W))
    val wrPtrGraySync2 = Reg(UInt((addrWidth+1).W))
    wrPtrGraySync1 := wrPtrGray
    wrPtrGraySync2 := wrPtrGraySync1

    // Empty condition: pointers equal
    io.empty := rdPtrGray === wrPtrGraySync2

    def grayToBinary(gray: UInt, width: Int): UInt = {
      val binary = Wire(Vec(width, UInt(1.W)))
      binary(width-1) = gray(width-1)
      for (i <- (width-2) to 0 by -1) {
        binary(i) = gray(i) ^ binary(i+1)
      }
      binary.asUInt
    }

    val wrPtrBinarySync = grayToBinary(wrPtrGraySync2, addrWidth+1)
    val rdPtrBinaryLocal = rdPtrBinary
    io.rdLevel := (wrPtrBinarySync - rdPtrBinaryLocal)(addrWidth-1, 0)
  }
}

// 7. 格雷码计数器 (Gray Code Counter)
// 用于异步FIFO的地址指针，避免多位同时翻转
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

// 8. 握手信号的异步包装 (Async Handshake Wrapper)
// 将单bit请求应答信号跨时钟域
class AsyncHandshake(dataType: UInt, dataWidth: Int = 32) extends Module {
  val io = IO(new Bundle {
    // Write side (send)
    val wrClk   = Input(Clock())
    val wrRst_n = Input(AsyncReset())
    val wrValid = Input(Bool())
    val wrReady = Output(Bool())
    val wrData  = Input(UInt(dataWidth.W))

    // Read side (recv)
    val rdClk   = Input(Clock())
    val rdRst_n = Input(AsyncReset())
    val rdValid = Output(Bool())
    val rdReady = Input(Bool())
    val rdData  = Output(UInt(dataWidth.W))
  })

  // 使用异步FIFO实现，深度为2^addrWidth
  val fifo = Module(new AsyncFifo(dataWidth, 2))
  fifo.io.wrClk   := io.wrClk
  fifo.io.wrRst_n := io.wrRst_n
  fifo.io.wrEn    := io.wrValid && !fifo.io.full
  fifo.io.din     := io.wrData
  io.wrReady      := !fifo.io.full

  fifo.io.rdClk   := io.rdClk
  fifo.io.rdRst_n := io.rdRst_n
  fifo.io.rdEn    := io.rdReady && !fifo.io.empty
  io.rdValid      := !fifo.io.empty
  io.rdData       := fifio.dout
}
