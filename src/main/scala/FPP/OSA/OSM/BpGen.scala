package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Backpressure generator (docs §3.12).
 *
 * Converts per-port occupancy into per-port per-PFC-priority backpressure
 * with hysteresis: BP asserts when occupancy > losslessThr and de-asserts
 * when occupancy < losslessThr - hysteresis (per port, gated by bpMask).
 */
class BpGen(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val occupancy  = Input(Vec(config.portCount, UInt(config.bufAddrWidth.W)))
    val thresholds = Input(Vec(config.portCount, new PortThresholds))
    val pfcPriMap  = Input(new PfcPriMap)
    val bpMask     = Input(Vec(config.portCount, Vec(config.maxPfcPriority, Bool())))
    val macBp      = Output(new BackpressureOutput(config))
  })

  val bpState = RegInit(VecInit(Seq.fill(config.portCount)(false.B)))
  for (p <- 0 until config.portCount) {
    val over  = io.occupancy(p) > io.thresholds(p).lossless
    val under = io.occupancy(p) < (io.thresholds(p).lossless - io.thresholds(p).hysteresis)
    when(over)      { bpState(p) := true.B }
    .elsewhen(under){ bpState(p) := false.B }
    for (pf <- 0 until config.maxPfcPriority) {
      io.macBp.bp(p)(pf) := bpState(p) && io.bpMask(p)(pf)
    }
  }
}
