package FPP.OSA.OSM

import circt.stage.ChiselStage

/** Elaboration entry: generate the OSA Verilog (RTL verification). */
object OSAGen extends App {
  ChiselStage.emitSystemVerilog(new OSATop(OSAConfig()))
}
