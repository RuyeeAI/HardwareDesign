package BaseCbb.utils

import chisel3._
import chisel3.util._

/** Galois LFSR pseudo-random number generator.
  *
  * Configurable width with built-in primitive polynomials. Outputs the full
  * LFSR state on each cycle when enabled.
  *
  * @param width LFSR width (8, 16, 24, or 32)
  */
class Lfsr(width: Int = 16) extends Module {
  val io = IO(new Bundle {
    val seed = Input(UInt(width.W))
    val load = Input(Bool())
    val en   = Input(Bool())
    val out  = Output(UInt(width.W))
  })

  // Primitive polynomials for maximum-length Galois LFSR
  // Tap value only includes bits that are XORed during feedback (excludes implicit x^width term)
  private val polyTap: Map[Int, BigInt] = Map(
    8  -> BigInt("10001011", 2),    // 0x8B: x^8 + x^6 + x^5 + x^4 + 1 (tap bits 7,5,4,3,1)
    16 -> BigInt("101101", 2),      // 0x2D: x^16 + x^5 + x^3 + x^2 + 1 (tap bits 4,2,1,0)
    24 -> BigInt("1000011011", 2),  // 0x20D: x^24 + x^4 + x^3 + x + 1 (tap bits 9,3,2,0)
    32 -> BigInt("110000000000000000000111", 2) // 0x60000007: x^32 + x^22 + x^2 + x + 1
  )

  private val tap = polyTap.getOrElse(width, BigInt("10011", 2))
  private val tapUInt = tap.U(width.W)

  val state = RegInit(1.U(width.W))

  when(io.load) {
    state := io.seed
  }.elsewhen(io.en) {
    // Galois LFSR: shift right, if LSB=1 then XOR with tap
    val lsb = state(0)
    val shifted = Cat(false.B, state(width - 1, 1))
    state := Mux(lsb, shifted ^ tapUInt, shifted)
  }

  io.out := state
}

object Lfsr {
  def apply(seed: UInt, load: Bool, en: Bool): UInt = {
    val mod = Module(new Lfsr(seed.getWidth))
    mod.io.seed := seed
    mod.io.load := load
    mod.io.en   := en
    mod.io.out
  }
}
