package BaseCbb.sequential

import chisel3._
import chisel3.util._

// N位同步寄存器
class Register(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val clk  = Input(Clock())
    val rst_n = Input(AsyncReset())
    val din  = Input(UInt(width.W))
    val wen  = Input(Bool())
    val dout = Output(UInt(width.W))
  })

  withClockAndReset(io.clk, io.rst_n) {
    val reg = Reg(UInt(width.W))
    when (io.wen) {
      reg := io.din
    }
    io.dout := reg
  }
}

// 1读1写寄存器堆
class RegFile1R1W(dataWidth: Int = 32, addrWidth: Int = 5) extends Module {
  val depth = 1 << addrWidth
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    // Write port
    val wen   = Input(Bool())
    val waddr = Input(UInt(addrWidth.W))
    val wdata = Input(UInt(dataWidth.W))
    // Read port
    val ren   = Input(Bool())
    val raddr = Input(UInt(addrWidth.W))
    val rdata = Output(UInt(dataWidth.W))
  })

  val mem = Mem(depth, UInt(dataWidth.W))

  io.rdata := 0.U
  when (io.ren) {
    io.rdata := mem.read(io.raddr)
  }

  withClock(io.clk) {
    when (io.wen) {
      mem.write(io.waddr, io.wdata)
    }
  }
}

// 2读1写寄存器堆
class RegFile2R1W(dataWidth: Int = 32, addrWidth: Int = 5) extends Module {
  val depth = 1 << addrWidth
  val io = IO(new Bundle {
    val clk    = Input(Clock())
    // Write port
    val wen    = Input(Bool())
    val waddr  = Input(UInt(addrWidth.W))
    val wdata  = Input(UInt(dataWidth.W))
    // Read ports
    val raddr1 = Input(UInt(addrWidth.W))
    val raddr2 = Input(UInt(addrWidth.W))
    val rdata1 = Output(UInt(dataWidth.W))
    val rdata2 = Output(UInt(dataWidth.W))
  })

  val mem = Mem(depth, UInt(dataWidth.W))

  io.rdata1 := mem.read(io.raddr1)
  io.rdata2 := mem.read(io.raddr2)

  withClock(io.clk) {
    when (io.wen) {
      mem.write(io.waddr, io.wdata)
    }
  }
}

// 二进制加法计数器
class UpCounter(width: Int = 8) extends Module {
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val rst_n = Input(AsyncReset())
    val en    = Input(Bool())
    val clear = Input(Bool())
    val count = Output(UInt(width.W))
    val carry = Output(Bool())
  })

  withClockAndReset(io.clk, io.rst_n) {
    val cnt = Reg(UInt(width.W))
    io.carry := cnt.andR & io.en

    when (!io.rst_n.asBool || io.clear) {
      cnt := 0.U
    } .elsewhen (io.en) {
      cnt := cnt + 1.U
    }
    io.count := cnt
  }
}

// 模N计数器
class ModNCounter(mod: Int = 100) extends Module {
  val width = log2Ceil(mod)
  val io = IO(new Bundle {
    val clk     = Input(Clock())
    val rst_n   = Input(AsyncReset())
    val en      = Input(Bool())
    val count   = Output(UInt(width.W))
    val overflow = Output(Bool())
  })

  withClockAndReset(io.clk, io.rst_n) {
    val cnt = Reg(UInt(width.W))
    io.overflow := (cnt === (mod - 1).U) && io.en

    when (!io.rst_n.asBool) {
      cnt := 0.U
    } .elsewhen (io.en) {
      when (io.overflow) {
        cnt := 0.U
      } .otherwise {
        cnt := cnt + 1.U
      }
    }
    io.count := cnt
  }
}

// 二分频
class ClkDiv2 extends Module {
  val io = IO(new Bundle {
    val clkIn  = Input(Clock())
    val rst_n  = Input(AsyncReset())
    val clkOut = Output(Clock())
  })

  withClockAndReset(io.clkIn, io.rst_n) {
    val clkReg = RegInit(false.B)
    clkReg := ~clkReg
    io.clkOut := clkReg.asClock
  }
}

// 奇数分频 (保证50%占空比)
class ClkDivOdd(div: Int = 3) extends Module {
  require(div % 2 == 1, "div must be odd")
  val half = (div - 1) / 2
  val cntWidth = log2Ceil(div)
  val io = IO(new Bundle {
    val clkIn  = Input(Clock())
    val rst_n  = Input(AsyncReset())
    val clkOut = Output(Clock())
  })

  val cntP = Reg(UInt(cntWidth.W))
  val cntN = Reg(UInt(cntWidth.W))
  val clkP = RegInit(false.B)
  val clkN = RegInit(false.B)

  // Positive edge counter
  withClockAndReset(io.clkIn, io.rst_n) {
    when (cntP === (div - 1).U) {
      cntP := 0.U
      clkP := false.B
    } .otherwise {
      cntP := cntP + 1.U
      when (cntP === half.U) {
        clkP := true.B
      }
    }
  }

  // Negative edge counter
  withClock((~io.clkIn.asUInt)(0).asClock) {
    when (!io.rst_n.asBool) {
      cntN := 0.U
      clkN := false.B
    } .elsewhen (cntN === (div - 1).U) {
      cntN := 0.U
      clkN := false.B
    } .otherwise {
      cntN := cntN + 1.U
      when (cntN === half.U) {
        clkN := true.B
      }
    }
  }

  io.clkOut := (clkP | clkN).asClock
}

// 通用整数分频
class ClkDiv(div: Int = 10) extends Module {
  val cntWidth = log2Ceil(div)
  val io = IO(new Bundle {
    val clkIn  = Input(Clock())
    val rst_n  = Input(AsyncReset())
    val clkOut = Output(Bool())
  })

  val half = div >> 1
  val cnt = Reg(UInt(cntWidth.W))
  val clkOutReg = RegInit(false.B)

  withClockAndReset(io.clkIn, io.rst_n) {
    when (cnt === (half - 1).U) {
      clkOutReg := ~clkOutReg
      cnt := 0.U
    } .otherwise {
      cnt := cnt + 1.U
    }
  }

  io.clkOut := clkOutReg
}

// 同步FIFO
class SyncFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module {
  val depth = 1 << addrWidth
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val rst_n = Input(AsyncReset())
    // Write
    val wrEn  = Input(Bool())
    val din   = Input(UInt(dataWidth.W))
    // Read
    val rdEn  = Input(Bool())
    val dout  = Output(UInt(dataWidth.W))
    // Status
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
    when (!io.rst_n.asBool) {
      wrPtr := 0.U
    } .elsewhen (io.wrEn && !io.full) {
      mem.write(wrPtr, io.din)
      wrPtr := wrPtr + 1.U
    }
  }

  // Read
  withClockAndReset(io.clk, io.rst_n) {
    when (!io.rst_n.asBool) {
      rdPtr := 0.U
      io.dout := 0.U
    } .elsewhen (io.rdEn && !io.empty) {
      io.dout := mem.read(rdPtr)
      rdPtr := rdPtr + 1.U
    }
  }

  // Count
  withClockAndReset(io.clk, io.rst_n) {
    when (!io.rst_n.asBool) {
      count := 0.U
    } .otherwise {
      switch (Cat(io.wrEn && !io.full, io.rdEn && !io.empty)) {
        is ("b10".U) { count := count + 1.U }
        is ("b01".U) { count := count - 1.U }
      }
    }
  }
}

// 三段式FSM模板
object FsmStates {
  val sIDLE :: sBUSY :: sDONE :: Nil = Enum(3)
}

class FsmTemplate(stateNum: Int = 4) extends Module {
  val stateWidth = log2Ceil(stateNum)
  val io = IO(new Bundle {
    val clk         = Input(Clock())
    val rst_n       = Input(AsyncReset())
    val start       = Input(Bool())
    val doneCond    = Input(Bool())
    val idle        = Output(Bool())
    val busy        = Output(Bool())
    val done        = Output(Bool())
    val currentState = Output(UInt(stateWidth.W))
  })

  val currentState = Reg(UInt(stateWidth.W))
  val nextState = Wire(UInt(stateWidth.W))

  import FsmStates._

  // Stage 1: State register
  withClockAndReset(io.clk, io.rst_n) {
    currentState := nextState
  }

  // Stage 2: Next state logic
  nextState := sIDLE
  switch (currentState) {
    is (sIDLE) {
      when (io.start) {
        nextState := sBUSY
      } .otherwise {
        nextState := sIDLE
      }
    }
    is (sBUSY) {
      when (io.doneCond) {
        nextState := sDONE
      } .otherwise {
        nextState := sBUSY
      }
    }
    is (sDONE) {
      nextState := sIDLE
    }
  }

  // Stage 3: Output logic
  io.idle   := currentState === sIDLE
  io.busy   := currentState === sBUSY
  io.done   := currentState === sDONE
  io.currentState := currentState
}
