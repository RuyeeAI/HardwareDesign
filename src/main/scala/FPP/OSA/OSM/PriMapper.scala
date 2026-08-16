package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Priority class mapper (docs §3.4).
 *
 * Maps the 4-bit OrgQindex from PPRS to a 2-bit priority class via a
 * 16-entry configurable LUT. Reset default: linear mapping
 * (OrgQindex[3:2] -> lossy/lossless, OrgQindex[1:0] -> low/high).
 */
class PriMapper extends GenModule {
  val io = IO(new Bundle {
    val orgQindex = Input(UInt(4.W))
    val priClass  = Output(UInt(2.W))
    val lutWrAddr = Input(UInt(4.W))
    val lutWrData = Input(UInt(2.W))
    val lutWrEn   = Input(Bool())
  })

  val lut = RegInit(VecInit(Seq.tabulate(16)(i => (i & 0x3).U(2.W))))
  when(io.lutWrEn) {
    lut(io.lutWrAddr) := io.lutWrData
  }
  io.priClass := lut(io.orgQindex)
}
