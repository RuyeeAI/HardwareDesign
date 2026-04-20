package BaseCbb.fifo

import chisel3._
import chisel3.util._

/**
 * 经典异步FIFO - 格雷码指针实现，标准输出有一拍延迟
 */
class AsyncFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module {
  val depth = 1 << addrWidth
  val io = IO(new Bundle {
    // Write domain
    val wrClk   = Input(Clock())
    val wrRst_n = Input(AsyncReset())
    val wrEn    = Input(Bool())
    val din     = Input(UInt(dataWidth.W))
    val full    = Output(Bool())
    val wrLevel = Output(UInt(addrWidth.W))

    // Read domain
    val rdClk   = Input(Clock())
    val rdRst_n = Input(AsyncReset())
    val rdEn    = Input(Bool())
    val dout    = Output(UInt(dataWidth.W))
    val empty   = Output(Bool())
    val rdLevel = Output(UInt(addrWidth.W))
  })

  // Helper: Gray to binary conversion
  def grayToBinary(gray: UInt, width: Int): UInt = {
    val binary = Wire(Vec(width, Bool()))
    binary(width - 1) := gray(width - 1)
    for (i <- (width - 2) to 0 by -1) {
      binary(i) := gray(i) ^ binary(i + 1)
    }
    binary.asUInt
  }

  // Memory array
  val mem = Mem(depth, UInt(dataWidth.W))

  // Wire cross-domain pointers
  val wrPtrGrayWire = Wire(UInt((addrWidth + 1).W))
  val rdPtrGrayWire = Wire(UInt((addrWidth + 1).W))

  // ========== Write Domain ==========
  withClockAndReset(io.wrClk, io.wrRst_n) {
    val wrPtrBin  = RegInit(0.U((addrWidth + 1).W))
    val wrPtrGray = RegInit(0.U((addrWidth + 1).W))
    wrPtrGrayWire := wrPtrGray

    when (io.wrEn && !io.full) {
      wrPtrBin  := wrPtrBin + 1.U
      wrPtrGray := (wrPtrBin + 1.U) ^ ((wrPtrBin + 1.U) >> 1)
      mem.write(wrPtrBin(addrWidth - 1, 0), io.din)
    }

    // Sync read pointer to write domain
    val rdGraySync1 = RegInit(0.U((addrWidth + 1).W))
    val rdGraySync2 = RegInit(0.U((addrWidth + 1).W))
    rdGraySync1 := rdPtrGrayWire
    rdGraySync2 := rdGraySync1

    val rdBinSync = grayToBinary(rdGraySync2, addrWidth + 1)
    io.wrLevel := (wrPtrBin - rdBinSync)(addrWidth - 1, 0)

    // Full: MSB and second MSB differ, lower bits same
    io.full := (wrPtrGray(addrWidth) =/= rdGraySync2(addrWidth)) &&
               (wrPtrGray(addrWidth - 1) =/= rdGraySync2(addrWidth - 1)) &&
               (wrPtrGray(addrWidth - 2, 0) === rdGraySync2(addrWidth - 2, 0))
  }

  // ========== Read Domain ==========
  withClockAndReset(io.rdClk, io.rdRst_n) {
    val rdPtrBin  = RegInit(0.U((addrWidth + 1).W))
    val rdPtrGray = RegInit(0.U((addrWidth + 1).W))
    rdPtrGrayWire := rdPtrGray

    when (io.rdEn && !io.empty) {
      rdPtrBin  := rdPtrBin + 1.U
      rdPtrGray := (rdPtrBin + 1.U) ^ ((rdPtrBin + 1.U) >> 1)
    }
    // Registered output
    io.dout := mem.read(rdPtrBin(addrWidth - 1, 0))

    // Sync write pointer to read domain
    val wrGraySync1 = RegInit(0.U((addrWidth + 1).W))
    val wrGraySync2 = RegInit(0.U((addrWidth + 1).W))
    wrGraySync1 := wrPtrGrayWire
    wrGraySync2 := wrGraySync1

    val wrBinSync = grayToBinary(wrGraySync2, addrWidth + 1)
    io.rdLevel := (wrBinSync - rdPtrBin)(addrWidth - 1, 0)
    io.empty := rdPtrGray === wrGraySync2
  }
}

/**
 * 异步零延时读取FIFO - 读地址组合逻辑输出，零延迟
 */
class AsyncZeroLatencyFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module {
  val depth = 1 << addrWidth
  val io = IO(new Bundle {
    // Write domain
    val wrClk   = Input(Clock())
    val wrRst_n = Input(AsyncReset())
    val wrEn    = Input(Bool())
    val din     = Input(UInt(dataWidth.W))
    val full    = Output(Bool())

    // Read domain
    val rdClk   = Input(Clock())
    val rdRst_n = Input(AsyncReset())
    val rdEn    = Input(Bool())
    val dout    = Output(UInt(dataWidth.W)) // Zero latency combinational output
    val empty   = Output(Bool())
  })

  // Helper: Gray to binary conversion
  def grayToBinary(gray: UInt, width: Int): UInt = {
    val binary = Wire(Vec(width, Bool()))
    binary(width - 1) := gray(width - 1)
    for (i <- (width - 2) to 0 by -1) {
      binary(i) := gray(i) ^ binary(i + 1)
    }
    binary.asUInt
  }

  val mem = Mem(depth, UInt(dataWidth.W))

  // Wire cross-domain pointers
  val wrPtrGrayWire = Wire(UInt((addrWidth + 1).W))
  val rdPtrGrayWire = Wire(UInt((addrWidth + 1).W))
  val rdPtrBinWire  = Wire(UInt((addrWidth + 1).W))

  // ========== Write Domain ==========
  withClockAndReset(io.wrClk, io.wrRst_n) {
    val wrPtrBin  = RegInit(0.U((addrWidth + 1).W))
    val wrPtrGray = RegInit(0.U((addrWidth + 1).W))
    wrPtrGrayWire := wrPtrGray

    when (io.wrEn && !io.full) {
      wrPtrBin  := wrPtrBin + 1.U
      wrPtrGray := (wrPtrBin + 1.U) ^ ((wrPtrBin + 1.U) >> 1)
      mem.write(wrPtrBin(addrWidth - 1, 0), io.din)
    }

    // Sync read pointer to write domain
    val rdGraySync1 = RegInit(0.U((addrWidth + 1).W))
    val rdGraySync2 = RegInit(0.U((addrWidth + 1).W))
    rdGraySync1 := rdPtrGrayWire
    rdGraySync2 := rdGraySync1

    val rdBinSync = grayToBinary(rdGraySync2, addrWidth + 1)

    // Full: MSB and second MSB differ, lower bits same
    io.full := (wrPtrGray(addrWidth) =/= rdGraySync2(addrWidth)) &&
               (wrPtrGray(addrWidth - 1) =/= rdGraySync2(addrWidth - 1)) &&
               (wrPtrGray(addrWidth - 2, 0) === rdGraySync2(addrWidth - 2, 0))
  }

  // ========== Read Domain ==========
  withClockAndReset(io.rdClk, io.rdRst_n) {
    val rdPtrBin  = RegInit(0.U((addrWidth + 1).W))
    val rdPtrGray = RegInit(0.U((addrWidth + 1).W))
    rdPtrGrayWire := rdPtrGray
    rdPtrBinWire  := rdPtrBin

    when (io.rdEn && !io.empty) {
      rdPtrBin  := rdPtrBin + 1.U
      rdPtrGray := (rdPtrBin + 1.U) ^ ((rdPtrBin + 1.U) >> 1)
    }

    // Sync write pointer to read domain
    val wrGraySync1 = RegInit(0.U((addrWidth + 1).W))
    val wrGraySync2 = RegInit(0.U((addrWidth + 1).W))
    wrGraySync1 := wrPtrGrayWire
    wrGraySync2 := wrGraySync1

    io.empty := rdPtrGray === wrGraySync2
  }

  // Zero latency combinational read
  io.dout := mem.read(rdPtrBinWire(addrWidth - 1, 0))
}
