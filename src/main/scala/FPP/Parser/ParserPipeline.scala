package FPP.Parser

import BaseCbb.data.GenBundle
import chisel3._

// ============= Pipeline Configuration =============

/** Case class to configure pipeline register insertion at each parsing stage */
case class ParserPipelineConfig(
  pipeAfterEth:    Boolean = false,
  pipeAfterVlan:   Boolean = false,
  pipeAfterMpls:   Boolean = false,
  pipeAfterIpv4:   Boolean = false,
  pipeAfterIpv6:   Boolean = false,
  pipeAfterTcp:    Boolean = false,
  pipeAfterUdp:    Boolean = false,
  pipeAfterIcmp:   Boolean = false,
  pipeAfterGre:    Boolean = false,
  pipeAfterVxlan:  Boolean = false,
  pipeAfterGeneve: Boolean = false,
  pipeAfterGtpu:   Boolean = false,
  pipeAfterArp:    Boolean = false
)

object ParserPipelineConfig {
  def default = ParserPipelineConfig()

  /** Preset: pipeline after major L3/L4 stages for timing */
  def aggressiveTiming = ParserPipelineConfig(
    pipeAfterEth = true,
    pipeAfterIpv4 = true,
    pipeAfterIpv6 = true,
    pipeAfterUdp = true
  )

  /** Preset: pipeline only after IPv4 for critical path relief */
  def mildTiming = ParserPipelineConfig(
    pipeAfterIpv4 = true
  )

  def hasAnyPipeline(cfg: ParserPipelineConfig): Boolean =
    cfg.pipeAfterEth || cfg.pipeAfterVlan || cfg.pipeAfterMpls ||
    cfg.pipeAfterIpv4 || cfg.pipeAfterIpv6 || cfg.pipeAfterTcp ||
    cfg.pipeAfterUdp || cfg.pipeAfterIcmp || cfg.pipeAfterGre ||
    cfg.pipeAfterVxlan || cfg.pipeAfterGeneve || cfg.pipeAfterGtpu ||
    cfg.pipeAfterArp
}

// ============= Stage Output Bundle =============

/** Output from a parser stage with metadata for pipeline decision */
class StageOutput extends GenBundle {
  val bytes = UInt(512.W)
  val meta = new ParseMeta
  val nextType = UInt(8.W)
  val headerLen = UInt(8.W)
  val valid = Bool()
}