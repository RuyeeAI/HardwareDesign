package BaseCbb.fifo

import chisel3._
import chisel3.util._
import BaseCbb.memory.{SpMemoryPort, TpMemoryPort}

/**
 * 基础同步FIFO — memory 移至模块外部通过 TpMemoryPort 连接。
 *
 * @param readLatency 读延迟：1 = 读输出为寄存器（一拍）；0 = 组合读（零延时，读地址预加 1）
 *                    （合并自 SyncZeroLatencyFifo，原独立类保留为兼容子类）
 */
class SyncFifo(
  dataWidth: Int = 32,
  addrWidth: Int = 4,
  readLatency: Int = 1
) extends Module {
  require(readLatency == 0 || readLatency == 1, s"readLatency must be 0 or 1, got $readLatency")
  val depth = 1 << addrWidth

  val io = IO(new Bundle {
    // Memory port — 连接外部 SRAM（双端口，支持同时读写不同地址）
    val mem = Flipped(new TpMemoryPort(addrWidth, dataWidth))

    val clk   = Input(Clock())
    val rst_n = Input(AsyncReset())
    val wrEn  = Input(Bool())
    val din   = Input(UInt(dataWidth.W))
    val rdEn  = Input(Bool())
    val dout  = Output(UInt(dataWidth.W))
    val empty = Output(Bool())
    val full  = Output(Bool())
    val level = Output(UInt(addrWidth.W))
  })

  val wrPtr = Reg(UInt(addrWidth.W))
  val rdPtr = Reg(UInt(addrWidth.W))
  val count = Reg(UInt((addrWidth + 1).W))

  io.empty := count === 0.U
  io.full  := count === depth.U
  io.level := count(addrWidth - 1, 0)

  val notRst = !io.rst_n.asBool

  // 读地址：readLatency=0 时预加 1（组合读下一数据），否则直接 rdPtr
  val nextRdAddr = Mux(io.rdEn && !io.empty, rdPtr + 1.U, rdPtr)
  val rdAddr = if (readLatency == 1) rdPtr else nextRdAddr

  // Drive memory port
  io.mem.we    := io.wrEn && !io.full
  io.mem.re    := io.rdEn && !io.empty
  io.mem.waddr := wrPtr
  io.mem.raddr := rdAddr
  io.mem.wdata := io.din

  // Write pointer
  withClockAndReset(io.clk, io.rst_n) {
    when(notRst) {
      wrPtr := 0.U
    }.elsewhen(io.wrEn && !io.full) {
      wrPtr := wrPtr + 1.U
    }
  }

  // Read: readLatency=1 时输出寄存（一拍延迟）；=0 时组合直通
  withClockAndReset(io.clk, io.rst_n) {
    if (readLatency == 1) {
      when(notRst) {
        rdPtr := 0.U
        io.dout := 0.U
      }.elsewhen(io.rdEn && !io.empty) {
        io.dout := io.mem.rdata
        rdPtr := rdPtr + 1.U
      }
    } else {
      when(notRst) {
        rdPtr := 0.U
      }.elsewhen(io.rdEn && !io.empty) {
        rdPtr := rdPtr + 1.U
      }
      io.dout := io.mem.rdata // zero-latency
    }
  }

  // Count update
  withClockAndReset(io.clk, io.rst_n) {
    when(notRst) {
      count := 0.U
    }.otherwise {
      switch(Cat(io.wrEn && !io.full, io.rdEn && !io.empty)) {
        is("b10".U) { count := count + 1.U }
        is("b01".U) { count := count - 1.U }
      }
    }
  }
}

/** 兼容子类：同步零延时读取FIFO（readLatency=0） */
class SyncZeroLatencyFifo(
  dataWidth: Int = 32,
  addrWidth: Int = 4
) extends SyncFifo(dataWidth, addrWidth, readLatency = 0)

/**
 * 寄存器搭建的小型FIFO — 使用寄存器堆，适合深度很小的场景（<=32）。
 * 无外部 memory 接口。
 */
class RegisterBasedFifo(
  dataWidth: Int = 32,
  depth:     Int = 8
) extends Module {
  require(depth <= 32, "register based Fifo depth is recommended to be small (<=32)")
  val addrWidth = log2Ceil(depth)

  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val rst_n = Input(AsyncReset())
    val wrEn  = Input(Bool())
    val din   = Input(UInt(dataWidth.W))
    val rdEn  = Input(Bool())
    val dout  = Output(UInt(dataWidth.W))
    val empty = Output(Bool())
    val full  = Output(Bool())
    val level = Output(UInt(addrWidth.W))
  })

  val regs = Reg(Vec(depth, UInt(dataWidth.W)))
  val wrPtr = Reg(UInt(addrWidth.W))
  val rdPtr = Reg(UInt(addrWidth.W))
  val count = Reg(UInt((addrWidth + 1).W))

  io.empty := count === 0.U
  io.full  := count === depth.U
  io.level := count(addrWidth - 1, 0)

  val notRst = !io.rst_n.asBool

  io.dout := regs(rdPtr)

  withClockAndReset(io.clk, io.rst_n) {
    when(notRst) {
      wrPtr := 0.U
      rdPtr := 0.U
      count := 0.U
    }.otherwise {
      when(io.wrEn && !io.full) {
        regs(wrPtr) := io.din
        wrPtr := wrPtr + 1.U
      }
      when(io.rdEn && !io.empty) {
        rdPtr := rdPtr + 1.U
      }
      switch(Cat(io.wrEn && !io.full, io.rdEn && !io.empty)) {
        is("b10".U) { count := count + 1.U }
        is("b01".U) { count := count - 1.U }
      }
    }
  }
}

/** FIFO with two single-port SRAMs and read/write conflict handling.
  *
  * When read and write target the same bank simultaneously:
  *   - Same address: bypass write data to read output (1-cycle latency)
  *   - Different address: stall the read (hold rdPtr, keep last dout)
  *
  * Otherwise operates identically to DualSinglePortRamFifo.
  */
class DualSPRamFifo(
  dataWidth: Int = 32,
  addrWidth: Int = 4
) extends Module {
  val depth     = 1 << addrWidth
  val halfDepth = depth >> 1
  require(addrWidth >= 1, "DualSPRamFifo requires addrWidth >= 1")

  val io = IO(new Bundle {
    val memBank0 = Flipped(new SpMemoryPort(addrWidth - 1, dataWidth))
    val memBank1 = Flipped(new SpMemoryPort(addrWidth - 1, dataWidth))

    val clk   = Input(Clock())
    val rst_n = Input(AsyncReset())
    val wrEn  = Input(Bool())
    val din   = Input(UInt(dataWidth.W))
    val rdEn  = Input(Bool())
    val dout  = Output(UInt(dataWidth.W))
    val empty = Output(Bool())
    val full  = Output(Bool())
    val level = Output(UInt(addrWidth.W))
  })

  val wrPtr = Reg(UInt(addrWidth.W))
  val rdPtr = Reg(UInt(addrWidth.W))
  val count = Reg(UInt((addrWidth + 1).W))

  io.empty := count === 0.U
  io.full  := count === depth.U
  io.level := count(addrWidth - 1, 0)

  val wrBank = wrPtr(addrWidth - 1)
  val rdBank = rdPtr(addrWidth - 1)
  val wrAddr = wrPtr(addrWidth - 2, 0)
  val rdAddr = rdPtr(addrWidth - 2, 0)

  val doWr = io.wrEn && !io.full
  val doRd = io.rdEn && !io.empty

  val wrToBank0 = doWr && wrBank === 0.U
  val wrToBank1 = doWr && wrBank === 1.U
  val rdFromBank0 = doRd && rdBank === 0.U
  val rdFromBank1 = doRd && rdBank === 1.U

  val conflictBank0 = wrToBank0 && rdFromBank0
  val conflictBank1 = wrToBank1 && rdFromBank1
  val sameAddr0 = conflictBank0 && wrAddr === rdAddr
  val sameAddr1 = conflictBank1 && wrAddr === rdAddr

  // Drive bank0 — stall read on conflict
  io.memBank0.we    := wrToBank0
  io.memBank0.re    := rdFromBank0 && !conflictBank0
  io.memBank0.addr  := Mux(wrToBank0, wrAddr, rdAddr)
  io.memBank0.wdata := io.din

  // Drive bank1 — stall read on conflict
  io.memBank1.we    := wrToBank1
  io.memBank1.re    := rdFromBank1 && !conflictBank1
  io.memBank1.addr  := Mux(wrToBank1, wrAddr, rdAddr)
  io.memBank1.wdata := io.din

  // Bypass write data on same-address conflict
  val bypassData  = RegEnable(io.din, sameAddr0 || sameAddr1)
  val bypassValid = RegNext(sameAddr0 || sameAddr1, false.B)

  val dout0 = Mux(bypassValid && rdBank === 0.U, bypassData, io.memBank0.rdata)
  val dout1 = Mux(bypassValid && rdBank === 1.U, bypassData, io.memBank1.rdata)
  io.dout := Mux(rdBank === 0.U, dout0, dout1)

  // Stall read on different-address conflict
  val rdStall   = (conflictBank0 && !sameAddr0) || (conflictBank1 && !sameAddr1)
  val rdAdvance = doRd && !rdStall

  val notRst = !io.rst_n.asBool

  withClockAndReset(io.clk, io.rst_n) {
    when(notRst) {
      wrPtr := 0.U
      rdPtr := 0.U
      count := 0.U
    }.otherwise {
      when(doWr) {
        wrPtr := wrPtr + 1.U
      }
      when(rdAdvance) {
        rdPtr := rdPtr + 1.U
      }
      switch(Cat(doWr, rdAdvance)) {
        is("b10".U) { count := count + 1.U }
        is("b01".U) { count := count - 1.U }
      }
    }
  }
}

/** 兼容子类：Ping-Pong 双单口 SRAM FIFO。
  * 行为升级为与 DualSPRamFifo 一致（含同 bank 读写冲突处理；
  * 原实现未处理冲突，行为未定义）。
  */
class DualSinglePortRamFifo(
  dataWidth: Int = 32,
  addrWidth: Int = 4
) extends DualSPRamFifo(dataWidth, addrWidth)
