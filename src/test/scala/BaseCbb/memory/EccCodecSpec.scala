package BaseCbb.memory

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * EccCodec 直连 harness（纯组合）：encode → 注入 flipEnc 指定的比特翻转 → decode。
 * 用于穷举验证 SECDED/Parity 的检错、纠错与回环，覆盖旧测试的盲区：
 *   - dataBits 不能被 eccSegNum 整除的多段布局（旧 decodeEccMultiSeg 偏移错位）；
 *   - 单比特错误的真实纠错通路（旧 wrap 级"虚拟注入"从未执行过纠错逻辑）。
 */
class EccCodecHarness(dataBits: Int, protectWidthTh: Int, protect: MemoryProtectType.Value) extends Module {

  private val eccSegNum       = math.ceil(dataBits.toDouble / protectWidthTh).toInt
  private val eccSegWidth     = math.ceil(dataBits.toDouble / eccSegNum).toInt
  private val lastEccSegWidth = dataBits - (eccSegNum - 1) * eccSegWidth

  /** 编码总宽（数据位 + 校验位），与 Memory.dataWidth 的公式一致 */
  val encWidth: Int = protect match {
    case MemoryProtectType.ProtNone => dataBits
    case MemoryProtectType.Parity   => dataBits + eccSegNum
    case MemoryProtectType.ECC =>
      dataBits + (0 until eccSegNum).map { i =>
        val sb = if (i < eccSegNum - 1) eccSegWidth else lastEccSegWidth
        EccCodec.eccWidthOf(sb) + 1
      }.sum
    case _ => dataBits
  }

  /**
   * ECC 各段在编码字中的比特区间 [start, end)。分段 SECDED 的意义即各段独立纠一比特：
   * 同段内双比特错不可纠（uerr=1），跨段双比特错可分别纠正（uerr=0）。
   */
  val eccSegRanges: Seq[(Int, Int)] = protect match {
    case MemoryProtectType.ECC =>
      val widths = (0 until eccSegNum).map { i =>
        val sb = if (i < eccSegNum - 1) eccSegWidth else lastEccSegWidth
        sb + EccCodec.eccWidthOf(sb) + 1
      }
      widths.scanLeft(0)(_ + _).sliding(2).map { case Seq(a, b) => (a, b) }.toSeq
    case _ => Seq((0, encWidth))
  }

  val io = IO(new Bundle {
    val in      = Input(UInt(dataBits.W))
    val flipEnc = Input(UInt(encWidth.W))
    val out     = Output(UInt(dataBits.W))
    val err     = Output(Bool())
    val uerr    = Output(Bool())
  })

  private val encoded = protect match {
    case MemoryProtectType.ProtNone => io.in
    case MemoryProtectType.Parity   => EccCodec.encodeParity(io.in, eccSegNum, eccSegWidth, lastEccSegWidth)
    case MemoryProtectType.ECC      => EccCodec.encodeEcc(io.in, eccSegNum, eccSegWidth, lastEccSegWidth)
    case _                          => io.in
  }
  private val corrupted = encoded ^ io.flipEnc
  private val (dec, err, uerr) =
    EccCodec.decodeAndCheck(corrupted, dataBits, protect, eccSegNum, eccSegWidth, lastEccSegWidth)

  io.out  := dec
  io.err  := err
  io.uerr := uerr
}

class EccCodecSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  behavior.of("EccCodec")

  /**
   * 对单一配置做完整检查：无错回环 + 编码位单比特穷举 + 双比特抽样。
   *
   * 语义约定（与 SpMemoryWrap3 一致）：
   *   - ECC：单比特错 → err=1 且数据被纠回（uerr=0）；双比特错 → uerr=1；
   *   - Parity：无纠错能力，任何检测到的错都算不可纠正（err=1 且 uerr=1），
   *     数据位翻转原样读出（坏数据 + 报错），校验位翻转不影响数据。
   */
  private def checkConfig(dataBits: Int, th: Int, protect: MemoryProtectType.Value, doubleBitSamples: Int): Unit = {
    val rnd = new scala.util.Random(0x5EED ^ dataBits ^ th)
    val info = s"(dataBits=$dataBits th=$th protect=$protect)"
    test(new EccCodecHarness(dataBits, th, protect)) { c =>
      val encWidth = c.encWidth

      // ── 无错回环 ────────────────────────────────────────────────
      for (_ <- 0 until 8) {
        val d = BigInt(dataBits, rnd)
        c.io.in.poke(d.U(dataBits.W))
        c.io.flipEnc.poke(0.U(encWidth.W))
        c.io.out.expect(d, s"roundtrip mismatch $info")
        c.io.err.expect(false.B, s"roundtrip flagged err $info")
        c.io.uerr.expect(false.B, s"roundtrip flagged uerr $info")
      }

      if (protect != MemoryProtectType.ProtNone) {
        // ── 单比特穷举：每个编码位（数据位/校验位/总校验位）翻转一次 ──
        for (bit <- 0 until encWidth) {
          val d = BigInt(dataBits, rnd)
          c.io.in.poke(d.U(dataBits.W))
          c.io.flipEnc.poke((BigInt(1) << bit).U(encWidth.W))
          protect match {
            case MemoryProtectType.ECC =>
              // SECDED：单比特错必须被纠回（含校验位/总校验位出错时数据本就未坏）
              c.io.out.expect(d, s"single-bit flip @enc[$bit] data not recovered $info")
              c.io.err.expect(true.B, s"single-bit flip @enc[$bit] not detected $info")
              c.io.uerr.expect(false.B, s"single-bit flip @enc[$bit] misclassified as uncorrectable $info")
            case MemoryProtectType.Parity =>
              // Parity 无纠错能力：数据位翻转原样读出（坏数据 + 报错），校验位翻转不影响数据
              val expectOut = if (bit < dataBits) d ^ (BigInt(1) << bit) else d
              c.io.out.expect(expectOut, s"parity flip @enc[$bit] data passthrough wrong $info")
              c.io.err.expect(true.B, s"parity flip @enc[$bit] not detected $info")
              c.io.uerr.expect(true.B, s"parity error should be uncorrectable $info")
            case _ =>
          }
        }
        // ── 双比特抽样（同段内）：ECC 必须报不可纠正 ─────────────────
        if (protect == MemoryProtectType.ECC) {
          val segs = c.eccSegRanges
          for (_ <- 0 until doubleBitSamples) {
            val (lo, hi) = segs(rnd.nextInt(segs.size))
            val b1 = lo + rnd.nextInt(hi - lo)
            var b2 = lo + rnd.nextInt(hi - lo)
            while (b2 == b1) { b2 = lo + rnd.nextInt(hi - lo) }
            val d = BigInt(dataBits, rnd)
            c.io.in.poke(d.U(dataBits.W))
            c.io.flipEnc.poke(((BigInt(1) << b1) | (BigInt(1) << b2)).U(encWidth.W))
            c.io.uerr.expect(true.B, s"same-segment double-bit flip @($b1,$b2) not flagged uerr $info")
          }
          // ── 双比特抽样（跨段）：各段独立纠错，数据应被完整恢复 ──────
          if (segs.size > 1) {
            for (_ <- 0 until doubleBitSamples) {
              val s1 = segs(rnd.nextInt(segs.size))
              var s2 = segs(rnd.nextInt(segs.size))
              while (s2 == s1) { s2 = segs(rnd.nextInt(segs.size)) }
              val b1 = s1._1 + rnd.nextInt(s1._2 - s1._1)
              val b2 = s2._1 + rnd.nextInt(s2._2 - s2._1)
              val d = BigInt(dataBits, rnd)
              c.io.in.poke(d.U(dataBits.W))
              c.io.flipEnc.poke(((BigInt(1) << b1) | (BigInt(1) << b2)).U(encWidth.W))
              c.io.out.expect(d, s"cross-segment double-bit flip @($b1,$b2) not corrected $info")
              c.io.err.expect(true.B, s"cross-segment double-bit flip @($b1,$b2) not detected $info")
              c.io.uerr.expect(false.B, s"cross-segment double-bit flip @($b1,$b2) should be correctable $info")
            }
          }
        }
      }
    }
  }

  // 经典配置（与既有 wrap 级测试一致：单段）
  "ECC single segment (32b/320)" should "pass exhaustive single-bit and double-bit checks" in
    checkConfig(32, 320, MemoryProtectType.ECC, doubleBitSamples = 64)

  "Parity single segment (32b/320)" should "pass exhaustive single-bit checks" in
    checkConfig(32, 320, MemoryProtectType.Parity, doubleBitSamples = 0)

  "ProtNone (32b/320)" should "pass roundtrip" in
    checkConfig(32, 320, MemoryProtectType.ProtNone, doubleBitSamples = 0)

  // 旧测试盲区：dataBits 与 eccSegNum 不整除 → 多段宽度不等
  //（旧 decodeEccMultiSeg 偏移错位、decodeParity 校验位布局错位均在此触发）
  "ECC 2 unequal segments (321b/320)" should "pass exhaustive single-bit and double-bit checks" in
    checkConfig(321, 320, MemoryProtectType.ECC, doubleBitSamples = 64)

  "Parity 2 segments (321b/320)" should "pass exhaustive single-bit checks" in
    checkConfig(321, 320, MemoryProtectType.Parity, doubleBitSamples = 0)

  "ECC 2 equal segments (100b/64)" should "pass exhaustive single-bit and double-bit checks" in
    checkConfig(100, 64, MemoryProtectType.ECC, doubleBitSamples = 64)

  "ECC 3 unequal segments (310b/128)" should "pass exhaustive single-bit and double-bit checks" in
    checkConfig(310, 128, MemoryProtectType.ECC, doubleBitSamples = 64)

  "ECC small segments (17b/8)" should "pass exhaustive single-bit and double-bit checks" in
    checkConfig(17, 8, MemoryProtectType.ECC, doubleBitSamples = 64)
}

/**
 * 物理内存路径（BlackBox 分支）此前从未被任何测试触及：
 * TpMemoryWrap 旧版把输出流水放在时钟域之外（RawModule 无隐式时钟，无法 elaborate）。
 * 本测试保证 isPhysicalMemory=true 两条路径均可正常生成 SystemVerilog。
 */
class MemoryWrapPhysicalElabSpec extends AnyFlatSpec with Matchers {

  "SpMemoryWrap/TpMemoryWrap with isPhysicalMemory=true" should "elaborate to SystemVerilog" in {
    val cfg = Memory(
      name = "PhysMemSample",
      dataType = UInt(64.W),
      depth = 32,
      protect = MemoryProtectType.ECC,
      flopIn = true,
      flopOut = true,
      isPhysicalMemory = true
    )
    val svSp = chisel3.stage.ChiselStage.emitSystemVerilog(new SpMemoryWrap(cfg))
    svSp should include("PhysMemSample")
    val svTp = chisel3.stage.ChiselStage.emitSystemVerilog(new TpMemoryWrap(cfg))
    svTp should include("PhysMemSample")
  }

  "Memory config validation" should "reject invalid geometry" in {
    an[IllegalArgumentException] should be thrownBy
      Memory(name = "bad", dataType = UInt(8.W), depth = 0)
    an[IllegalArgumentException] should be thrownBy
      Memory(name = "bad", dataType = UInt(8.W), depth = 4, protectWidthTh = 2)
  }
}
