package BaseCbb.utils

import chisel3._
import chisel3.util._

/** Compress valid elements from the input array to the LSB side of the output.
  *
  * Example: in=[A,B,C,D,E], valid=10101 -> out=[A,C,E,0,0], count=3
  *
  * @param gen type template for array elements
  * @param n   number of elements in the array
  */
class Compress[T <: Data](gen: T, val n: Int) extends Module {
  val io = IO(new Bundle {
    val in    = Input(Vec(n, gen))
    val valid = Input(UInt(n.W))
    val out   = Output(Vec(n, gen))
    val count = Output(UInt((log2Ceil(n + 1)).W))
  })

  private val countWidth = log2Ceil(n + 1)

  // Convert valid bits to integers for prefix sum
  private val validInts = io.valid.asBools.map(v => Mux(v, 1.U(countWidth.W), 0.U(countWidth.W)))

  // Tree-based parallel prefix sum (inclusive scan), O(log N) stages
  private var psum = validInts
  private var step = 1
  while (step < n) {
    psum = (0 until n).map { i =>
      if (i >= step) psum(i) + psum(i - step) else psum(i)
    }
    step *= 2
  }

  // Route each valid input to its destination (zero-based index)
  private val default = 0.U.asTypeOf(gen)
  for (j <- 0 until n) {
    io.out(j) := MuxCase(default,
      (0 until n).map { i =>
        val dest = psum(i) - 1.U
        (io.valid(i) && dest === j.U) -> io.in(i)
      }
    )
  }

  io.count := psum.last
}

object Compress {
  /** Factory that instantiates a Compress module.
    *
    * @param in    input data vector
    * @param valid valid bitmask (1 = element is valid)
    * @return (compressed output vector, count of valid elements)
    */
  def apply[T <: Data](in: Vec[T], valid: UInt): (Vec[T], UInt) = {
    val mod = Module(new Compress(in.head.cloneType, in.length))
    mod.io.in := in
    mod.io.valid := valid
    (mod.io.out, mod.io.count)
  }
}

/** Scatter (inverse of Compress): place packed input elements at positions
  * indicated by a mask.
  *
  * Example: in=[A,B,C], mask=10101 -> out=[A,0,B,0,C]
  *
  * @param gen type template for array elements
  * @param n   number of elements in the array
  */
class Scatter[T <: Data](gen: T, val n: Int) extends Module {
  val io = IO(new Bundle {
    val in   = Input(Vec(n, gen))
    val mask = Input(UInt(n.W))
    val out  = Output(Vec(n, gen))
  })

  private val countWidth = log2Ceil(n + 1)

  private val maskInts = io.mask.asBools.map(m => Mux(m, 1.U(countWidth.W), 0.U(countWidth.W)))

  private var psum = maskInts
  private var step = 1
  while (step < n) {
    psum = (0 until n).map { i =>
      if (i >= step) psum(i) + psum(i - step) else psum(i)
    }
    step *= 2
  }

  // For each output position, route the corresponding packed input or zero
  private val default = 0.U.asTypeOf(gen)
  for (j <- 0 until n) {
    io.out(j) := MuxCase(default,
      (0 until n).map { i =>
        (io.mask(j) && (psum(j) - 1.U === i.U)) -> io.in(i)
      }
    )
  }
}

object Scatter {
  /** Factory that instantiates a Scatter module.
    *
    * @param in   packed input data vector
    * @param mask destination bitmask (1 = place an element here)
    * @return scattered output vector
    */
  def apply[T <: Data](in: Vec[T], mask: UInt): Vec[T] = {
    val mod = Module(new Scatter(in.head.cloneType, in.length))
    mod.io.in := in
    mod.io.mask := mask
    mod.io.out
  }
}
