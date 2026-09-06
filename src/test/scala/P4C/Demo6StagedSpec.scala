package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** demo6-deepchain 切拍等价性测试（P0）：
  * Demo6DeepchainIngress（N=1 基线）vs Demo6DeepchainStagedIngress（N=4 切拍）。
  *
  * 时序契约：io.valid 为单拍脉冲（每个脉冲发起一次调用），流水 N 级，末级 stageValid
  * （io.outValid）拍提交一次状态写；相邻脉冲间隔 ≥ N。
  * 语义注：stats.write(8w0, meta.acc) 按 P4 语义读 action 执行前的 meta.acc 输入值，
  * 故 stats(0) 终值 = 脉冲时的 io.metaIn.acc（而非加法链和）；加法链和体现在 metaOut.acc。
  */
class Demo6StagedSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  private val Mask16 = (BigInt(1) << 16) - 1

  private def sum16(vals: Seq[BigInt]): BigInt = vals.foldLeft(BigInt(0))(_ + _) & Mask16

  private def pokeMeta(c: Demo6DeepchainIngress, vals: Seq[BigInt], accIn: BigInt): Unit =
    pokeMetaElems(c.io.metaIn.elements, vals, accIn)

  private def pokeMeta(c: Demo6DeepchainStagedIngress, vals: Seq[BigInt], accIn: BigInt): Unit =
    pokeMetaElems(c.io.metaIn.elements, vals, accIn)

  private def pokeMetaElems(elems: Map[String, _ <: chisel3.Data], vals: Seq[BigInt], accIn: BigInt): Unit = {
    elems.foreach { case (name, data) =>
      if (name.startsWith("f") && name.drop(1).forall(_.isDigit))
        data.asInstanceOf[UInt].poke(vals(name.drop(1).toInt).U(16.W))
    }
    elems("acc").asInstanceOf[UInt].poke(accIn.U(16.W))
  }

  private def valsOf(seed: Int): Seq[BigInt] =
    (0 until 16).map(i => BigInt((i * 4099 + seed * 257 + 13) & 0xffff))

  behavior.of("Demo6DeepchainIngress（N=1 基线，由 demo6-deepchain.p4 生成）")

  it should "valid 脉冲单拍完成：加法链组合输出 + stats(0)=输入 acc + count" in {
    test(new Demo6DeepchainIngress) { c =>
      val vals = valsOf(1)
      val accIn = BigInt(0x1234)
      pokeMeta(c, vals, accIn)
      c.io.valid.poke(true.B)
      c.clock.step(1)
      c.io.metaOut.acc.expect(sum16(vals).U(16.W))
      c.io.ex_stats(0).expect(accIn.U(16.W))
      c.io.ex_hits(0).expect(1.U(32.W))
    }
  }

  behavior.of("Demo6DeepchainStagedIngress（N=4 切拍，15 级加法链均分 4 级）")

  it should "4 拍流水：末级 outValid 脉冲、输出就绪、状态在末级拍提交一次" in {
    test(new Demo6DeepchainStagedIngress) { c =>
      val vals = valsOf(1)
      val accIn = BigInt(0x1234)
      pokeMeta(c, vals, accIn)
      c.io.valid.poke(true.B) // 脉冲（第 0 拍）
      c.clock.step(1)
      c.io.valid.poke(false.B)
      c.io.outValid.expect(false.B) // 第 1 拍
      c.clock.step(1)
      c.io.outValid.expect(false.B) // 第 2 拍
      c.clock.step(1)
      c.io.outValid.expect(true.B) // 第 3 拍（末级 sV_3）
      c.io.metaOut.acc.expect(sum16(vals).U(16.W))
      c.io.ex_stats(0).expect(0.U(16.W)) // 状态在末级拍结束沿才提交
      c.io.ex_hits(0).expect(0.U(32.W))
      c.clock.step(1)
      c.io.outValid.expect(false.B) // 单拍脉冲
      c.io.ex_stats(0).expect(accIn.U(16.W))
      c.io.ex_hits(0).expect(1.U(32.W))
    }
  }

  it should "valid 低电平期间流水不推进、无虚假写" in {
    test(new Demo6DeepchainStagedIngress) { c =>
      val vals = valsOf(2)
      val accIn = BigInt(0x00aa)
      pokeMeta(c, vals, accIn)
      c.io.valid.poke(true.B)
      c.clock.step(1)
      c.io.valid.poke(false.B)
      c.clock.step(3) // 第 3 拍末提交，第 4 拍可见
      c.io.ex_stats(0).expect(accIn.U(16.W))
      c.io.ex_hits(0).expect(1.U(32.W))
      // valid=0 持续 ≥ N 拍并更换输入：无任何写
      pokeMeta(c, valsOf(99), BigInt(0xffff))
      c.clock.step(4)
      c.io.ex_stats(0).expect(accIn.U(16.W))
      c.io.ex_hits(0).expect(1.U(32.W))
      c.io.outValid.expect(false.B)
    }
  }

  it should "与 N=1 基线功能等价（多轮调用，状态累加一致，容忍 N-1 拍延迟）" in {
    val rounds = Seq(valsOf(1), valsOf(2), valsOf(3))
    val accIns = Seq(BigInt(100), BigInt(200), BigInt(0x4321))
    // N=1 基线
    val baseline = scala.collection.mutable.ArrayBuffer.empty[BigInt]
    test(new Demo6DeepchainIngress) { c =>
      rounds.zip(accIns).foreach { case (vals, accIn) =>
        pokeMeta(c, vals, accIn)
        c.io.valid.poke(true.B)
        c.clock.step(1)
        baseline += c.io.ex_stats(0).peek().litValue
        c.io.valid.poke(false.B)
        c.clock.step(1)
      }
    }
    // N=4 切拍：每轮单拍脉冲，相邻脉冲间隔 4 拍（= N，满足发起间隔契约）
    var round = 0
    test(new Demo6DeepchainStagedIngress) { c =>
      rounds.zip(accIns).foreach { case (vals, accIn) =>
        pokeMeta(c, vals, accIn)
        c.io.valid.poke(true.B)
        c.clock.step(1)
        c.io.valid.poke(false.B)
        c.clock.step(3) // 脉冲后第 4 拍：写已提交
        c.io.ex_stats(0).expect(baseline(round).U(16.W))
        c.io.ex_hits(0).expect((round + 1).U(32.W))
        round += 1
      }
    }
  }
}
