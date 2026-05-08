package BaseCbb.arithmetic

import chisel3._
import chisel3.util._
import BaseCbb.basic.FullAdd

// N位逐位进位加法器 (N-bit Ripple Carry Adder)
class RippleCarryAdder(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val a    = Input(UInt(width.W))
    val b    = Input(UInt(width.W))
    val cin  = Input(Bool())
    val sum  = Output(UInt(width.W))
    val cout = Output(Bool())
  })

  val carry = Wire(Vec(width + 1, Bool()))
  carry(0) := io.cin
  io.cout := carry(width)

  val sumBits = Wire(Vec(width, Bool()))
  for (i <- 0 until width) {
    val add = Module(new FullAdd())
    add.io.a := io.a(i)
    add.io.b := io.b(i)
    add.io.cin := carry(i)
    sumBits(i) := add.io.sum
    carry(i + 1) := add.io.cout
  }
  io.sum := sumBits.asUInt
}

// N位进位选择加法器 (N-bit Carry Select Adder)
class CarrySelectAdder(width: Int = 32, blockSize: Int = 4) extends Module {
  val io = IO(new Bundle {
    val a    = Input(UInt(width.W))
    val b    = Input(UInt(width.W))
    val cin  = Input(Bool())
    val sum  = Output(UInt(width.W))
    val cout = Output(Bool())
  })

  val numBlocks = (width + blockSize - 1) / blockSize
  val carry = Wire(Vec(numBlocks + 1, Bool()))
  carry(0) := io.cin
  io.cout := carry(numBlocks)

  val blockSums = (0 until numBlocks).map { i =>
    val start = i * blockSize
    val end = Math.min((i + 1) * blockSize - 1, width - 1)
    val blockWidth = end - start + 1

    val sum0 = Wire(UInt(blockWidth.W))
    val sum1 = Wire(UInt(blockWidth.W))
    val cout0 = Wire(Bool())
    val cout1 = Wire(Bool())

    val rca0 = Module(new RippleCarryAdder(blockWidth))
    rca0.io.a := io.a(end, start)
    rca0.io.b := io.b(end, start)
    rca0.io.cin := false.B
    sum0 := rca0.io.sum
    cout0 := rca0.io.cout

    val rca1 = Module(new RippleCarryAdder(blockWidth))
    rca1.io.a := io.a(end, start)
    rca1.io.b := io.b(end, start)
    rca1.io.cin := true.B
    sum1 := rca1.io.sum
    cout1 := rca1.io.cout

    carry(i + 1) := Mux(carry(i), cout1, cout0)
    Mux(carry(i), sum1, sum0)
  }
  io.sum := Cat(blockSums.reverse)
}

// N位减法器
class Subtractor(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val a         = Input(UInt(width.W))
    val b         = Input(UInt(width.W))
    val diff      = Output(UInt(width.W))
    val borrowOut = Output(Bool())
  })

  val bNot = ~io.b
  val add = Module(new RippleCarryAdder(width))
  add.io.a := io.a
  add.io.b := bNot
  add.io.cin := true.B
  io.diff := add.io.sum
  io.borrowOut := add.io.cout
}

// N位加法/减法器
class AddSub(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val a      = Input(UInt(width.W))
    val b      = Input(UInt(width.W))
    val sub    = Input(Bool()) // 1 = subtract, 0 = add
    val result = Output(UInt(width.W))
    val cout   = Output(Bool())
  })

  val bXor = io.b ^ Fill(width, io.sub)
  val add = Module(new RippleCarryAdder(width))
  add.io.a := io.a
  add.io.b := bXor
  add.io.cin := io.sub
  io.result := add.io.sum
  io.cout := add.io.cout
}

// N位比较器
class Comparator(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val a  = Input(UInt(width.W))
    val b  = Input(UInt(width.W))
    val eq = Output(Bool())  // a == b
    val gt = Output(Bool())  // a > b
    val lt = Output(Bool())  // a < b
  })
  io.eq := io.a === io.b
  io.gt := io.a > io.b
  io.lt := io.a < io.b
}

// N位乘法器 (行为级)
class Multipler(widthA: Int = 16, widthB: Int = 16) extends Module {
  val outWidth = widthA + widthB
  val io = IO(new Bundle {
    val a       = Input(UInt(widthA.W))
    val b       = Input(UInt(widthB.W))
    val product = Output(UInt(outWidth.W))
  })
  io.product := io.a * io.b
}

// 左移位器
class LeftShifter(width: Int = 32) extends Module {
  val shiftWidth = log2Ceil(width)
  val io = IO(new Bundle {
    val din   = Input(UInt(width.W))
    val shamt = Input(UInt(shiftWidth.W))
    val dout  = Output(UInt(width.W))
  })
  io.dout := (io.din << io.shamt)
}

// 右移位器
class RightShifter(width: Int = 32, arithmetic: Boolean = false) extends Module {
  val shiftWidth = log2Ceil(width)
  val io = IO(new Bundle {
    val din   = Input(UInt(width.W))
    val shamt = Input(UInt(shiftWidth.W))
    val dout  = Output(UInt(width.W))
  })

  if (arithmetic) {
    io.dout := (io.din.asSInt >> io.shamt).asUInt
  } else {
    io.dout := (io.din >> io.shamt)
  }
}
