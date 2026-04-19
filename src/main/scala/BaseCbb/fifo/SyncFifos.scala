package BaseCbb.fifo

import chisel3._
import chisel3.util._

/**
 * 基础同步FIFO - 输出打拍，读延迟一拍
 */
class SyncFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module {
  val depth = 1 << addrWidth
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

  val mem = Mem(depth, UInt(dataWidth.W))
  val wrPtr = Reg(UInt(addrWidth.W))
  val rdPtr = Reg(UInt(addrWidth.W))
  val count = Reg(UInt((addrWidth + 1).W))

  io.empty := count === 0.U
  io.full  := count === depth.U
  io.level := count

  // Write
  withClockAndReset(io.clk, io.rst_n) {
    when (!io.rst_n) {
      wrPtr := 0.U
    } .elsewhen (io.wrEn && !io.full) {
      mem.write(wrPtr, io.din)
      wrPtr := wrPtr + 1.U
    }
  }

  // Read (registered output)
  withClockAndReset(io.clk, io.rst_n) {
    when (!io.rst_n) {
      rdPtr := 0.U
      io.dout := 0.U
    } .elsewhen (io.rdEn && !io.empty) {
      io.dout := mem.read(rdPtr)
      rdPtr := rdPtr + 1.U
    }
  }

  // Count update
  withClockAndReset(io.clk, io.rst_n) {
    when (!io.rst_n) {
      count := 0.U
    } .otherwise {
      switch (Cat(io.wrEn && !io.full, io.rdEn && !io.empty)) {
        is ("b10".U) { count := count + 1.U }
        is ("b01".U) { count := count - 1.U }
      }
    }
  }
}

/**
 * 同步零延时读取FIFO - 异步读，读地址改变立刻得到数据
 */
class SyncZeroLatencyFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module {
  val depth = 1 << addrWidth
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val rst_n = Input(AsyncReset())
    val wrEn  = Input(Bool())
    val din   = Input(UInt(dataWidth.W))
    val rdEn  = Input(Bool())
    val dout  = Output(UInt(dataWidth.W)) // 组合逻辑输出，零延迟
    val empty = Output(Bool())
    val full  = Output(Bool())
    val level = Output(UInt(addrWidth.W))
  })

  val mem = Mem(depth, UInt(dataWidth.W))
  val wrPtr = Reg(UInt(addrWidth.W))
  val rdPtr = Reg(UInt(addrWidth.W))
  val count = Reg(UInt((addrWidth + 1).W))

  io.empty := count === 0.U
  io.full  := count === depth.U
  io.level := count

  // Zero latency read: combinational read from memory
  io.dout := mem.read(rdPtr)

  // Write
  withClockAndReset(io.clk, io.rst_n) {
    when (!io.rst_n) {
      wrPtr := 0.U
    } .elsewhen (io.wrEn && !io.full) {
      mem.write(wrPtr, io.din)
      wrPtr := wrPtr + 1.U
    }
  }

  // Read pointer update
  withClockAndReset(io.clk, io.rst_n) {
    when (!io.rst_n) {
      rdPtr := 0.U
      count := 0.U
    } .elsewhen (io.rdEn && !io.empty) {
      rdPtr := rdPtr + 1.U
    }

    // Count update
    when (!io.rst_n) {
      count := 0.U
    } .otherwise {
      switch (Cat(io.wrEn && !io.full, io.rdEn && !io.empty)) {
        is ("b10".U) { count := count + 1.U }
        is ("b01".U) { count := count - 1.U }
      }
    }
  }
}

/**
 * 寄存器搭建的小型FIFO - 使用寄存器堆而不是SRAM，适合深度很小的FIFO
 */
class RegisterBasedFifo(dataWidth: Int = 32, depth: Int = 8) extends Module {
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

  // Use registers instead of memory
  val regs = Reg(Vec(depth, UInt(dataWidth.W)))
  val wrPtr = Reg(UInt(addrWidth.W))
  val rdPtr = Reg(UInt(addrWidth.W))
  val count = Reg(UInt((addrWidth + 1).W))

  io.empty := count === 0.U
  io.full  := count === depth.U
  io.level := count

  // Combinational read out (zero latency from pointer change)
  io.dout := regs(rdPtr)

  withClockAndReset(io.clk, io.rst_n) {
    when (!io.rst_n) {
      wrPtr := 0.U
      rdPtr := 0.U
      count := 0.U
    } .otherwise {
      // Write
      when (io.wrEn && !io.full) {
        regs(wrPtr) := io.din
        wrPtr := wrPtr + 1.U
      }
      // Read
      when (io.rdEn && !io.empty) {
        rdPtr := rdPtr + 1.U
      }
      // Count
      switch (Cat(io.wrEn && !io.full, io.rdEn && !io.empty)) {
        is ("b10".U) { count := count + 1.U }
        is ("b01".U) { count := count - 1.U }
      }
    }
  }
}

/**
 * 双单口SRAM搭建的FIFO - Ping-Pong 结构，适合大深度FIFO
 * 使用两块单口RAM做读写并发，避免真正的双口RAM
 */
class DualSinglePortRamFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module {
  val depth = 1 << addrWidth
  val halfDepth = depth >> 1
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

  // Two single-port memories
  val memBank0 = Mem(halfDepth, UInt(dataWidth.W))
  val memBank1 = Mem(halfDepth, UInt(dataWidth.W))

  val wrPtr = Reg(UInt(addrWidth.W))
  val rdPtr = Reg(UInt(addrWidth.W))
  val count = Reg(UInt((addrWidth + 1).W))

  io.empty := count === 0.U
  io.full  := count === depth.U
  io.level := count

  // Select which bank to write/read
  val wrBank = wrPtr(addrWidth - 1)
  val rdBank = rdPtr(addrWidth - 1)
  val wrAddr = wrPtr(addrWidth - 2, 0)
  val rdAddr = rdPtr(addrWidth - 2, 0)

  // Write to selected bank
  when (io.wrEn && !io.full) {
    when (wrBank === 0.U) {
      memBank0.write(wrAddr, io.din)
    } .otherwise {
      memBank1.write(wrAddr, io.din)
    }
  }

  // Read from selected bank (combinational output)
  val dout0 = memBank0.read(rdAddr)
  val dout1 = memBank1.read(rdAddr)
  io.dout := Mux(rdBank === 0.U, dout0, dout1)

  // Pointers update
  withClockAndReset(io.clk, io.rst_n) {
    when (!io.rst_n) {
      wrPtr := 0.U
      rdPtr := 0.U
      count := 0.U
    } .otherwise {
      when (io.wrEn && !io.full) {
        wrPtr := wrPtr + 1.U
      }
      when (io.rdEn && !io.empty) {
        rdPtr := rdPtr + 1.U
      }
      // Count update
      switch (Cat(io.wrEn && !io.full, io.rdEn && !io.empty)) {
        is ("b10".U) { count := count + 1.U }
        is ("b01".U) { count := count - 1.U }
      }
    }
  }
}
