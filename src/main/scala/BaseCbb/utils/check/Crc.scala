package BaseCbb.utils.check

import chisel3._
import chisel3.util._

/** LFSR-based CRC generator.
  *
  * Processes one byte per cycle. Uses standard LFSR architecture: when a bit
  * shifted out is 1, XOR the polynomial into the remainder.
  *
  * @param polyWidth width of CRC polynomial (8, 16, 32)
  * @param poly      generator polynomial (default CRC-32 Ethernet 0x04C11DB7)
  */
class Crc(polyWidth: Int = 32, poly: BigInt = 0x04C11DB7L) extends Module {
  val io = IO(new Bundle {
    val data  = Input(UInt(8.W))
    val valid = Input(Bool())
    val first = Input(Bool())
    val init  = Input(UInt(polyWidth.W))
    val crc   = Output(UInt(polyWidth.W))
  })

  val remainder = RegInit(0.U(polyWidth.W))

  when(io.valid) {
    when(io.first) {
      remainder := io.init
    }
    // Shift each data bit through the LFSR, MSB first
    val initState = Mux(io.first && io.valid, io.init, remainder)
    val stages = (0 until 8).foldLeft(initState) { (state, i) =>
      val bit = io.data(7 - i) ^ state(polyWidth - 1)
      val shifted = Cat(state(polyWidth - 2, 0), 0.U(1.W))
      Mux(bit.asBool, shifted ^ poly.U(polyWidth.W), shifted)
    }
    remainder := stages
  }

  io.crc := remainder
}

object Crc {
  def apply(data: UInt, valid: Bool, first: Bool, init: UInt, poly: BigInt = 0x04C11DB7L): UInt = {
    val mod = Module(new Crc(init.getWidth, poly))
    mod.io.data  := data
    mod.io.valid := valid
    mod.io.first := first
    mod.io.init  := init
    mod.io.crc
  }
}

/** ICRC (Inverted CRC) calculator.
  *
  * Computes CRC(data ++ CRC(data)). The result should be a constant
  * remainder (e.g. 0xC704DD7B for CRC-32 Ethernet), useful for
  * error-detection at the receiver.
  */
class Icrc(polyWidth: Int = 32, poly: BigInt = 0x04C11DB7L) extends Module {
  val io = IO(new Bundle {
    val data    = Input(UInt(8.W))
    val valid   = Input(Bool())
    val first   = Input(Bool())
    val init    = Input(UInt(polyWidth.W))
    val crcIn   = Input(UInt(polyWidth.W))
    val crcVld  = Input(Bool())
    val icrc    = Output(UInt(polyWidth.W))
  })

  val crc = Module(new Crc(polyWidth, poly))
  crc.io.data  := Mux(io.crcVld, io.crcIn(7, 0), io.data)
  crc.io.valid := io.valid || io.crcVld
  crc.io.first := io.first
  crc.io.init  := io.init

  io.icrc := crc.io.crc
}

object Icrc {
  def apply(data: UInt, valid: Bool, first: Bool, init: UInt, crcIn: UInt, crcVld: Bool,
            poly: BigInt = 0x04C11DB7L): UInt = {
    val mod = Module(new Icrc(init.getWidth, poly))
    mod.io.data   := data
    mod.io.valid  := valid
    mod.io.first  := first
    mod.io.init   := init
    mod.io.crcIn  := crcIn
    mod.io.crcVld := crcVld
    mod.io.icrc
  }
}
