package BaseCbb.fifo

import chisel3._
import chisel3.util._
import BaseCbb.memory.TpMemoryPort

/**
 * 经典异步FIFO — 格雷码指针实现，memory 移至模块外部通过 TpMemoryPort 连接。
 *
 * @param readLatency 读延迟：1 = 读输出为 rdClk 域一拍寄存器；0 = 组合读（零延时，读地址预加 1）
 *                    （合并自 AsyncZeroLatencyFifo，原独立类保留为兼容子类）
 */
class AsyncFifo(
  dataWidth: Int = 32,
  addrWidth: Int = 4,
  readLatency: Int = 1
) extends Module {
  require(readLatency == 0 || readLatency == 1, s"readLatency must be 0 or 1, got $readLatency")
  val depth = 1 << addrWidth

  val io = IO(new Bundle {
    // Memory port — 连接外部 SRAM
    val mem = Flipped(new TpMemoryPort(addrWidth, dataWidth))

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

  def grayToBinary(gray: UInt, width: Int): UInt = {
    val binary = Wire(Vec(width, Bool()))
    binary(width - 1) := gray(width - 1)
    for (i <- (width - 2) to 0 by -1) {
      binary(i) := gray(i) ^ binary(i + 1)
    }
    binary.asUInt
  }

  // Cross-domain gray pointer wires
  val wrPtrGrayWire = Wire(UInt((addrWidth + 1).W))
  val rdPtrGrayWire = Wire(UInt((addrWidth + 1).W))

  // ========================================================================
  // Write Domain
  // ========================================================================
  withClockAndReset(io.wrClk, io.wrRst_n) {
    val wrPtrBin  = RegInit(0.U((addrWidth + 1).W))
    val wrPtrGray = RegInit(0.U((addrWidth + 1).W))

    wrPtrGrayWire := wrPtrGray

    // Drive memory port — write side
    io.mem.we    := io.wrEn && !io.full
    io.mem.waddr := wrPtrBin(addrWidth - 1, 0)
    io.mem.wdata := io.din

    when(io.wrEn && !io.full) {
      wrPtrBin  := wrPtrBin + 1.U
      wrPtrGray := (wrPtrBin + 1.U) ^ ((wrPtrBin + 1.U) >> 1)
    }

    // Sync read gray pointer to write domain (2-ff synchronizer)
    val rdGraySync1 = RegInit(0.U((addrWidth + 1).W))
    val rdGraySync2 = RegInit(0.U((addrWidth + 1).W))
    rdGraySync1 := rdPtrGrayWire
    rdGraySync2 := rdGraySync1

    val rdBinSync = grayToBinary(rdGraySync2, addrWidth + 1)
    io.wrLevel := (wrPtrBin - rdBinSync)(addrWidth - 1, 0)

    // Full: MSB and second MSB differ, lower bits same
    io.full := (wrPtrGray(addrWidth)     =/= rdGraySync2(addrWidth)) &&
               (wrPtrGray(addrWidth - 1) =/= rdGraySync2(addrWidth - 1)) &&
               (wrPtrGray(addrWidth - 2, 0) === rdGraySync2(addrWidth - 2, 0))
  }

  // ========================================================================
  // Read Domain
  // ========================================================================
  withClockAndReset(io.rdClk, io.rdRst_n) {
    val rdPtrBin  = RegInit(0.U((addrWidth + 1).W))
    val rdPtrGray = RegInit(0.U((addrWidth + 1).W))

    rdPtrGrayWire := rdPtrGray

    // 读地址：readLatency=0 时预加 1（组合读下一槽），否则直接 rdPtrBin
    val nextRdAddr = Mux(io.rdEn && !io.empty,
                         rdPtrBin(addrWidth - 1, 0) + 1.U,
                         rdPtrBin(addrWidth - 1, 0))

    // Drive memory port — read side
    io.mem.re    := io.rdEn && !io.empty
    io.mem.raddr := (if (readLatency == 1) rdPtrBin(addrWidth - 1, 0) else nextRdAddr)

    if (readLatency == 1) {
      // Registered output: one rdClk cycle latency
      io.dout := RegEnable(io.mem.rdata, io.mem.re)
    } else {
      // Zero-latency: rdata from memory directly to dout
      io.dout := io.mem.rdata
    }

    when(io.rdEn && !io.empty) {
      rdPtrBin  := rdPtrBin + 1.U
      rdPtrGray := (rdPtrBin + 1.U) ^ ((rdPtrBin + 1.U) >> 1)
    }

    // Sync write gray pointer to read domain (2-ff synchronizer)
    val wrGraySync1 = RegInit(0.U((addrWidth + 1).W))
    val wrGraySync2 = RegInit(0.U((addrWidth + 1).W))
    wrGraySync1 := wrPtrGrayWire
    wrGraySync2 := wrGraySync1

    val wrBinSync = grayToBinary(wrGraySync2, addrWidth + 1)
    io.rdLevel := (wrBinSync - rdPtrBin)(addrWidth - 1, 0)
    io.empty := rdPtrGray === wrGraySync2
  }
}

/** 兼容子类：异步零延时读取FIFO（readLatency=0） */
class AsyncZeroLatencyFifo(
  dataWidth: Int = 32,
  addrWidth: Int = 4
) extends AsyncFifo(dataWidth, addrWidth, readLatency = 0)
