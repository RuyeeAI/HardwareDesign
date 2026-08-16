package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Beat assembler (docs §3.11).
 *
 * Formats the <= 24-segment read bus into a 2 x 96B beat. No packet packing
 * inside a 96B unit: a unit's segments all belong to the same packet
 * (unit-aligned packets, final unit padded by byteEn).
 */
class CellAsm(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val rdData  = Flipped(Valid(new BufReadDataVec(config)))
    val cellOut = Decoupled(new CellOutputBundle(config))
    val desc    = Flipped(Valid(new PacketDesc))   // OBI source at SOP unit
  })

  val U = config.unitSegs          // 12 segments per 96B unit
  val K = config.outUnitsPerBeat   // 2 units

  val beat = Wire(new CellOutputBundle(config))
  for (u <- 0 until K) {
    val base = u * U
    var unitSop = false.B
    var unitEop = false.B
    for (s <- 0 until U) {
      val i = base + s
      beat.units(u).data(s)   := io.rdData.bits.segs(i).data(7, 0)
      beat.units(u).valid(s)  := io.rdData.bits.segs(i).valid
      beat.units(u).byteEn(s) := io.rdData.bits.segs(i).byteEn
      unitSop = unitSop || io.rdData.bits.segs(i).isSOP
      unitEop = unitEop || io.rdData.bits.segs(i).isEOP
    }
    beat.units(u).sop   := unitSop
    beat.units(u).eop   := unitEop
    beat.units(u).error := false.B
  }
  beat.portId := 0.U
  beat.lbo    := false.B
  beat.obi.valid := false.B
  beat.obi.bits := 0.U.asTypeOf(new OutOfBandInfo)

  io.cellOut.bits := beat
  io.cellOut.valid := io.rdData.valid
}
