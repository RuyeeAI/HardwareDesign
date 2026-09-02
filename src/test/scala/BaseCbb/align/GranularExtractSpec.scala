package BaseCbb.align

import chisel3._
import chisel3.util.log2Ceil
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

class GranularExtractSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // 黄金模型：out = (in >> (off*G)) 的低 M bit
  def golden(in: BigInt, off: Int, M: Int, G: Int): BigInt =
    (in >> (off * G)) & ((BigInt(1) << M) - 1)

  private val cases = Seq(
    (64, 32, 8), (64, 32, 16), (128, 32, 8), (256, 64, 32),
    (512, 96, 8), (512, 256, 64), (1024, 64, 32), (1024, 256, 64)
  )

  // ---- 1. 黄金模型回环（覆盖多种 N/M/G，均满足 G|N、G|M）----
  for ((n, m, g) <- cases) {
    s"GranularExtractAuto($n,$m,$g) matches golden model (in >> off*G)(M-1,0)" should "hold" in {
      test(new GranularExtractAuto(n, m, g)) { dut =>
        val S = (n - m) / g + 1
        val rnd = new Random(42)
        for (_ <- 0 until 200) {
          val in = BigInt(n, rnd)
          val off = rnd.nextInt(S)
          dut.io.in.poke(in.U(n.W))
          dut.io.off.poke(off.U(dut.io.off.getWidth.W))
          dut.io.out.expect(golden(in, off, m, g).U(m.W), s"off=$off")
        }
      }
    }
  }

  // ---- 2. 自动选择决策（统一用等效 mux2 计数：tree 在绝大多数参数下更小）----
  "auto selects tree(B) when S is large (512/96/8, S=53)" should "choose tree" in {
    test(new GranularExtractAuto(512, 96, 8)) { dut => dut.chosenImpl shouldBe "tree(B)" }
  }
  "auto selects tree(B) even at small S (64/32/8, S=5)" should "choose tree" in {
    test(new GranularExtractAuto(64, 32, 8)) { dut => dut.chosenImpl shouldBe "tree(B)" }
  }
  "auto selects bitmap(T2) only when S is tiny (64/32/16, S=3)" should "choose bitmap" in {
    test(new GranularExtractAuto(64, 32, 16)) { dut => dut.chosenImpl shouldBe "bitmap(T2)" }
  }
  "prefer=tree forces tree, prefer=bitmap forces bitmap" should "override auto" in {
    test(new GranularExtractAuto(64, 32, 8, prefer = "tree")) { dut => dut.chosenImpl shouldBe "tree(B)" }
    test(new GranularExtractAuto(512, 96, 8, prefer = "bitmap")) { dut => dut.chosenImpl shouldBe "bitmap(T2)" }
  }

  // ---- 3. 两种实现输出完全一致 ----
  "tree and bitmap produce identical output (512/96/8)" should "match" in {
    test(new ExtractCmpHarness(512, 96, 8)) { dut =>
      val rnd = new Random(7)
      for (_ <- 0 until 300) {
        val in = BigInt(512, rnd)
        val off = rnd.nextInt(53)
        dut.io.in.poke(in.U(512.W)); dut.io.off.poke(off.U(dut.io.off.getWidth.W))
        dut.io.outTree.expect(dut.io.outBitmap.peek(), s"mismatch off=$off")
      }
    }
  }
  "tree and bitmap produce identical output (64/32/8)" should "match" in {
    test(new ExtractCmpHarness(64, 32, 8)) { dut =>
      val rnd = new Random(7)
      for (_ <- 0 until 200) {
        val in = BigInt(64, rnd)
        val off = rnd.nextInt(5)
        dut.io.in.poke(in.U(64.W)); dut.io.off.poke(off.U(dut.io.off.getWidth.W))
        dut.io.outTree.expect(dut.io.outBitmap.peek(), s"mismatch off=$off")
      }
    }
  }

  // ---- 4. sideband（per-chunk 1 位标志）随同一网络移位 ----
  "sideband follows the same shift network (256/64/32, n=8, m=2)" should "align" in {
    test(new GranularExtractAuto(256, 64, 32)) { dut =>
      val rnd = new Random(11)
      for (_ <- 0 until 200) {
        val in = BigInt(256, rnd)
        val off = rnd.nextInt((256 - 64) / 32 + 1)
        val sb = BigInt(8, rnd) // sideIn 宽 n=8
        dut.io.in.poke(in.U(256.W)); dut.io.off.poke(off.U(dut.io.off.getWidth.W))
        dut.io.sideIn.poke(sb.U(8.W))
        dut.io.out.expect(golden(in, off, 64, 32).U(64.W))
        dut.io.sideOut.expect(golden(sb, off, 2, 1).U(2.W)) // 按 chunk 移位：G=1
      }
    }
  }
}

/** 对比两种实现是否一致的 harness。 */
class ExtractCmpHarness(N: Int, M: Int, G: Int) extends Module {
  private val K = log2Ceil((N - M) / G + 1)
  val io = IO(new Bundle {
    val in = Input(UInt(N.W))
    val off = Input(UInt(K.W))
    val outTree = Output(UInt(M.W))
    val outBitmap = Output(UInt(M.W))
  })
  val t = Module(new GranularExtractTree(N, M, G)); t.io.in := io.in; t.io.off := io.off; io.outTree := t.io.out
  val b = Module(new GranularExtractBitmap(N, M, G)); b.io.in := io.in; b.io.off := io.off; io.outBitmap := b.io.out
}
