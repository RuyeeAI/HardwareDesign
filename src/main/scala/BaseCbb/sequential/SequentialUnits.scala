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

// 参数化寄存器堆（nRead 读口 + nWrite 写口，Mem 实现）
class RegFile(
  nRead: Int = 1,
  nWrite: Int = 1,
  dataWidth: Int = 32,
  addrWidth: Int = 5
) extends Module {
  require(nRead >= 1 && nWrite >= 1, "RegFile requires at least 1 read and 1 write port")
  val depth = 1 << addrWidth
  val io = IO(new Bundle {
    val clk    = Input(Clock())
    // Write ports
    val wen    = Vec(nWrite, Input(Bool()))
    val waddr  = Vec(nWrite, Input(UInt(addrWidth.W)))
    val wdata  = Vec(nWrite, Input(UInt(dataWidth.W)))
    // Read ports
    val ren    = Vec(nRead, Input(Bool()))
    val raddr  = Vec(nRead, Input(UInt(addrWidth.W)))
    val rdata  = Vec(nRead, Output(UInt(dataWidth.W)))
  })

  val mem = Mem(depth, UInt(dataWidth.W))

  for (i <- 0 until nRead) {
    io.rdata(i) := 0.U
    when (io.ren(i)) {
      io.rdata(i) := mem.read(io.raddr(i))
    }
  }

  withClock(io.clk) {
    for (i <- 0 until nWrite) {
      when (io.wen(i)) {
        mem.write(io.waddr(i), io.wdata(i))
      }
    }
  }
}

// 1读1写寄存器堆（兼容包装，内部复用参数化 RegFile）
class RegFile1R1W(dataWidth: Int = 32, addrWidth: Int = 5) extends Module {
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val wen   = Input(Bool())
    val waddr = Input(UInt(addrWidth.W))
    val wdata = Input(UInt(dataWidth.W))
    val ren   = Input(Bool())
    val raddr = Input(UInt(addrWidth.W))
    val rdata = Output(UInt(dataWidth.W))
  })

  val rf = Module(new RegFile(1, 1, dataWidth, addrWidth))
  rf.io.clk := io.clk
  rf.io.wen(0) := io.wen
  rf.io.waddr(0) := io.waddr
  rf.io.wdata(0) := io.wdata
  rf.io.ren(0) := io.ren
  rf.io.raddr(0) := io.raddr
  io.rdata := rf.io.rdata(0)
}

// 2读1写寄存器堆（兼容包装）
class RegFile2R1W(dataWidth: Int = 32, addrWidth: Int = 5) extends Module {
  val io = IO(new Bundle {
    val clk    = Input(Clock())
    val wen    = Input(Bool())
    val waddr  = Input(UInt(addrWidth.W))
    val wdata  = Input(UInt(dataWidth.W))
    val raddr1 = Input(UInt(addrWidth.W))
    val raddr2 = Input(UInt(addrWidth.W))
    val rdata1 = Output(UInt(dataWidth.W))
    val rdata2 = Output(UInt(dataWidth.W))
  })

  val rf = Module(new RegFile(2, 1, dataWidth, addrWidth))
  rf.io.clk := io.clk
  rf.io.wen(0) := io.wen
  rf.io.waddr(0) := io.waddr
  rf.io.wdata(0) := io.wdata
  rf.io.ren(0) := true.B
  rf.io.ren(1) := true.B
  rf.io.raddr(0) := io.raddr1
  rf.io.raddr(1) := io.raddr2
  io.rdata1 := rf.io.rdata(0)
  io.rdata2 := rf.io.rdata(1)
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

// 模N计数器（内部复用 utils.ZCounter 计数逻辑单元，消除重复实现）
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
    val (cnt, wrap) = BaseCbb.utils.math.ZCounter(io.en, mod)
    io.count := cnt
    io.overflow := wrap
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

// 三段式FSM模板
object FsmStates {
  val sIDLE :: sBUSY :: sDONE :: Nil = Enum(3)
}

class FsmTemplate extends Module {
  // 三段式 FSM 模板：固定三态（sIDLE/sBUSY/sDONE）
  // （修复：原 stateNum 参数与写死的三态逻辑矛盾，已删除）
  val stateWidth = 2
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
