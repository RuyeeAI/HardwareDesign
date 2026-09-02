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
 *
 * 修复记录：desc 原先是悬空输入（OSATop 恒给 false.B），beat.portId 硬编码 0、
 * obi.valid 恒为 0，出口报文既没有端口标记也没有带外信息。现改为：
 *  - portId 取自当前描述符
 *  - 报文首拍（firstBeat）随 beat 输出 OBI（macHeader/pktId/orgQindex/priClass/byteCount）
 */
class CellAsm(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val rdData    = Flipped(Valid(new BufReadDataVec(config)))
    val cellOut   = Decoupled(new CellOutputBundle(config))
    val desc      = Flipped(Valid(new PacketDesc))  // 当前报文描述符（整包期间保持）
    val firstBeat = Input(Bool())                   // 本拍是报文首拍（OBI 来源）
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
      beat.units(u).byteEn(s) := Mux(io.rdData.bits.segs(i).valid,
                                    io.rdData.bits.segs(i).byteEn, 0.U)
      unitSop = unitSop || io.rdData.bits.segs(i).isSOP
      unitEop = unitEop || io.rdData.bits.segs(i).isEOP
    }
    beat.units(u).sop   := unitSop
    beat.units(u).eop   := unitEop
    beat.units(u).error := false.B
  }
  beat.portId := io.desc.bits.portId
  beat.lbo    := false.B

  // 首拍携带带外信息（时间戳在真实设计中由入口打拍，此处留 0）
  beat.obi.valid            := io.desc.valid && io.firstBeat
  beat.obi.bits.macHeader   := io.desc.bits.macHeader
  beat.obi.bits.portId      := io.desc.bits.portId
  beat.obi.bits.pktId       := io.desc.bits.pktId
  beat.obi.bits.orgQindex   := io.desc.bits.orgQindex
  beat.obi.bits.priClass    := io.desc.bits.priClass
  beat.obi.bits.byteCount   := io.desc.bits.byteCount
  beat.obi.bits.timestamp   := 0.U

  io.cellOut.bits := beat
  io.cellOut.valid := io.rdData.valid
}
