package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** demo7-directive 切拍指示测试（E2 端到端，chiseltest）：
  *   - Demo7DirectiveStagedFast（声明级指示 `// p4c: stages=2`）：恒 2 级流水，
  *     outValid 在脉冲后第 1 拍；
  *   - Demo7DirectiveStagedSlow（无指示，走全局默认 P4C_STAGED_STAGES=4）：4 级流水，
  *     outValid 在脉冲后第 3 拍；
  *   - 两者计算同一函数（f0..f7 之和），交叉验证跨拍数行为等价。
  *
  * 时序契约：io.valid 单拍脉冲，流水 n 级，末级 stageValid（io.outValid）拍输出就绪。
  */
class Demo7DirectiveSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  private val Mask16 = (BigInt(1) << 16) - 1

  private def sum16(vals: Seq[BigInt]): BigInt = vals.foldLeft(BigInt(0))(_ + _) & Mask16

  private def valsOf(seed: Int): Seq[BigInt] =
    (0 until 8).map(i => BigInt((i * 4099 + seed * 257 + 13) & 0xffff))

  private def pokeMeta(elements: Map[String, _ <: chisel3.Data], vals: Seq[BigInt]): Unit =
    elements.foreach { case (name, data) =>
      if (name.startsWith("f") && name.drop(1).forall(_.isDigit))
        data.asInstanceOf[UInt].poke(vals(name.drop(1).toInt).U(16.W))
    }

  behavior.of("Demo7DirectiveStagedFast（声明级指示 stages=2）")

  it should "拍数符合指示值 2：outValid 在脉冲后第 1 拍（且不晚于第 1 拍）" in {
    test(new Demo7DirectiveStagedFast) { c =>
      pokeMeta(c.io.metaIn.elements, valsOf(1))
      c.io.valid.poke(true.B) // 第 0 拍
      c.clock.step(1)
      c.io.valid.poke(false.B)
      c.io.outValid.expect(true.B) // 第 1 拍 = 末级 sV_1（2 级流水）
      c.io.metaOut.acc.expect(sum16(valsOf(1)).U(16.W)) // 末级组合直出
      c.clock.step(1)
      c.io.outValid.expect(false.B) // 单拍脉冲（排除拍数 > 2 的可能）
    }
  }

  behavior.of("Demo7DirectiveStagedSlow（无指示，全局默认 4）")

  it should "拍数符合全局默认 4：outValid 在脉冲后第 3 拍" in {
    test(new Demo7DirectiveStagedSlow) { c =>
      pokeMeta(c.io.metaIn.elements, valsOf(1))
      c.io.valid.poke(true.B)
      c.clock.step(1)
      c.io.valid.poke(false.B)
      c.io.outValid.expect(false.B) // 第 1 拍
      c.clock.step(1)
      c.io.outValid.expect(false.B) // 第 2 拍
      c.clock.step(1)
      c.io.outValid.expect(true.B) // 第 3 拍 = 末级 sV_3（4 级流水）
      c.io.metaOut.acc.expect(sum16(valsOf(1)).U(16.W))
      c.clock.step(1)
      c.io.outValid.expect(false.B)
    }
  }

  behavior.of("Fast（2 级）与 Slow（4 级）行为等价")

  it should "同一函数跨拍数等价：多轮随机输入，输出逐轮一致" in {
    val rounds = (1 to 4).map(valsOf)
    val expected = rounds.map(sum16)
    test(new Demo7DirectiveStagedFast) { c =>
      rounds.zipWithIndex.foreach { case (vals, r) =>
        pokeMeta(c.io.metaIn.elements, vals)
        c.io.valid.poke(true.B)
        c.clock.step(1)
        c.io.valid.poke(false.B)
        c.clock.step(1) // 2 级流水：脉冲后第 2 拍输出已过末级
        c.io.metaOut.acc.expect(expected(r).U(16.W))
      }
    }
    test(new Demo7DirectiveStagedSlow) { c =>
      rounds.zipWithIndex.foreach { case (vals, r) =>
        pokeMeta(c.io.metaIn.elements, vals)
        c.io.valid.poke(true.B)
        c.clock.step(1)
        c.io.valid.poke(false.B)
        c.clock.step(3) // 4 级流水：脉冲后第 4 拍输出已过末级
        c.io.metaOut.acc.expect(expected(r).U(16.W))
      }
    }
  }
}
