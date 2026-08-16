package FPP.OSA.OSM

import chisel3.stage.ChiselStage

/** Elaboration entry: generate the OSA Verilog (RTL verification). */
object OSAGen extends App {
  (new ChiselStage).emitVerilog(new OSATop(OSAConfig()), args)
}
