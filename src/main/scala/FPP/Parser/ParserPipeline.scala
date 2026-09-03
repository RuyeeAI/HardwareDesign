package FPP.Parser

import BaseCbb.data.{GenBundle, GenModule}
import chisel3._
import chisel3.util._

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

// ============= Pipeline Stage Wrapper =============

/**
 * Single-stage registered pipeline.
 *
 * 下游是 `Valid`（没有反压），所以这里的出口按"每拍必被取走"处理：
 * 本质是一条 1 拍延迟线（in.ready 恒 1，out.valid = 上一拍的 in.valid）。
 * 时序上的收益来自把组合逻辑切断，而不是靠反压来攒数据。
 */
class PipelineStage[T <: Data](gen: T) extends GenModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(gen))
    val out = DecoupledIO(gen)
  })

  val validReg = RegInit(false.B)
  val bitsReg = Reg(chiselTypeOf(io.in.bits))

  io.in.ready := true.B
  when(io.in.valid) {
    validReg := true.B
    bitsReg := io.in.bits
  }.otherwise {
    validReg := false.B
  }

  io.out.valid := validReg
  io.out.bits := bitsReg
}

object PipelineStage {
  def apply[T <: Data](in: DecoupledIO[T], enable: Boolean): DecoupledIO[T] = {
    if (enable) {
      val pipe = Module(new PipelineStage(in.bits.cloneType))
      pipe.io.in <> in
      pipe.io.out.ready := true.B
      pipe.io.out
    } else {
      in.ready := true.B
      in
    }
  }
}
