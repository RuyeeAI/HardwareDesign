package cbb.basic

import chisel3._
import chisel3.util._

// 1. 反相器 (Inverter)
class Inv extends Module {
  val io = IO(new Bundle {
    val in  = Input(Bool())
    val out = Output(Bool())
  })
  io.out := !io.in
}

// 2. 缓冲器 (Buffer)
class Buf extends Module {
  val io = IO(new Bundle {
    val in  = Input(Bool())
    val out = Output(Bool())
  })
  io.out := io.in
}

// 3. 二输入与门 (2-input AND Gate)
class And2 extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val y = Output(Bool())
  })
  io.y := io.a & io.b
}

// 4. 三输入与门 (3-input AND Gate)
class And3 extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val c = Input(Bool())
    val y = Output(Bool())
  })
  io.y := io.a & io.b & io.c
}

// 5. 二输入与非门 (2-input NAND Gate)
class Nand2 extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val y = Output(Bool())
  })
  io.y := !(io.a & io.b)
}

// 6. 三输入与非门 (3-input NAND Gate)
class Nand3 extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val c = Input(Bool())
    val y = Output(Bool())
  })
  io.y := !(io.a & io.b & io.c)
}

// 7. 二输入或门 (2-input OR Gate)
class Or2 extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val y = Output(Bool())
  })
  io.y := io.a | io.b
}

// 8. 二输入或非门 (2-input NOR Gate)
class Nor2 extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val y = Output(Bool())
  })
  io.y := !(io.a | io.b)
}

// 9. 三输入或非门 (3-input NOR Gate)
class Nor3 extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val c = Input(Bool())
    val y = Output(Bool())
  })
  io.y := !(io.a | io.b | io.c)
}

// 10. 二输入异或门 (2-input XOR Gate)
class Xor2 extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val y = Output(Bool())
  })
  io.y := io.a ^ io.b
}

// 11. 二输入异或非门 (2-input XNOR Gate)
class Xnor2 extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val y = Output(Bool())
  })
  io.y := !(io.a ^ io.b)
}

// 12. 二选一多路选择器 (2-to-1 MUX)
class Mux2 extends Module {
  val io = IO(new Bundle {
    val d0 = Input(Bool())
    val d1 = Input(Bool())
    val sel = Input(Bool())
    val y = Output(Bool())
  })
  io.y := Mux(io.sel, io.d1, io.d0)
}

// 13. N位宽二选一多路选择器
class Mux2N[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val d0 = Input(gen)
    val d1 = Input(gen)
    val sel = Input(Bool())
    val y = Output(gen)
  })
  io.y := Mux(io.sel, io.d1, io.d0)
}

// 14. 2-4译码器
class Dec2 extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(2.W))
    val out = Output(UInt(4.W))
  })
  io.out := 1.U << io.in
}

// 15. 3-8译码器
class Dec3 extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(3.W))
    val out = Output(UInt(8.W))
  })
  io.out := 1.U << io.in
}

// 16. D锁存器 (D Latch)
class DLatch extends Module {
  val io = IO(new Bundle {
    val d  = Input(Bool())
    val en = Input(Bool())
    val q  = Output(Bool())
  })
  when (io.en) {
    io.q := io.d
  }
}

// 17. 上升沿触发D触发器
class DFF extends Module {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val d   = Input(Bool())
    val q   = Output(Bool())
  })
  withClock(io.clk) {
    val qReg = RegNext(io.d)
    io.q := qReg
  }
}

// 带异步复位D触发器
class DFFAsyncRst extends Module {
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val rst_n = Input(AsyncReset())
    val d     = Input(Bool())
    val q     = Output(Bool())
  })
  withClockAndReset(io.clk, io.rst_n) {
    val qReg = RegInit(false.B)
    qReg := io.d
    io.q := qReg
  }
}

// 带同步复位D触发器
class DFFSyncRst extends Module {
  val io = IO(new Bundle {
    val clk   = Input(Clock())
    val rst_n = Input(Reset())
    val d     = Input(Bool())
    val q     = Output(Bool())
  })
  withClockAndReset(io.clk, io.rst_n) {
    val qReg = RegInit(false.B)
    qReg := io.d
    io.q := qReg
  }
}

// 半加器
class HalfAdd extends Module {
  val io = IO(new Bundle {
    val a    = Input(Bool())
    val b    = Input(Bool())
    val sum  = Output(Bool())
    val cout = Output(Bool())
  })
  io.sum  := io.a ^ io.b
  io.cout := io.a & io.b
}

// 全加器
class FullAdd extends Module {
  val io = IO(new Bundle {
    val a    = Input(Bool())
    val b    = Input(Bool())
    val cin  = Input(Bool())
    val sum  = Output(Bool())
    val cout = Output(Bool())
  })
  io.sum  := io.a ^ io.b ^ io.cin
  io.cout := (io.a & io.b) | (io.a & io.cin) | (io.b & io.cin)
}

// 基本SR锁存器
class SRLatch extends Module {
  val io = IO(new Bundle {
    val s  = Input(Bool())
    val r  = Input(Bool())
    val q  = Output(Bool())
    val qn = Output(Bool())
  })
  io.q  := !(io.s | io.qn)
  io.qn := !(io.r | io.q)
}

// 时钟门控单元
class ClockGating extends Module {
  val io = IO(new Bundle {
    val clk  = Input(Clock())
    val en   = Input(Bool())
    val gclk = Output(Clock())
  })
  // 基于锁存器的时钟门控
  val latchEn = Wire(Bool())
  latchEn := io.en
  when (!io.clk.asUInt.asBool) {
    latchEn := io.en
  }
  val gatedClk = io.clk.asUInt & latchEn
  io.gclk := gatedClk.asClock
}

// 二输入与或非门 AOI22
class AOI22 extends Module {
  val io = IO(new Bundle {
    val a1 = Input(Bool())
    val a2 = Input(Bool())
    val b1 = Input(Bool())
    val b2 = Input(Bool())
    val y  = Output(Bool())
  })
  io.y := !((io.a1 & io.a2) | (io.b1 & io.b2))
}

// 三输入与或非门 AOI32
class AOI32 extends Module {
  val io = IO(new Bundle {
    val a1 = Input(Bool())
    val a2 = Input(Bool())
    val a3 = Input(Bool())
    val b1 = Input(Bool())
    val b2 = Input(Bool())
    val y  = Output(Bool())
  })
  io.y := !((io.a1 & io.a2 & io.a3) | (io.b1 & io.b2))
}
