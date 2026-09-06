package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** 切拍补充验证（QA 第二层独立验证，覆盖工程师未写的角度）：
  *   1. Demo4ExternStaged（E1 加权下 N=4）与 N=1 基线 Demo4ExternIngress 的多轮连续 valid 脉冲
  *      等价性（RMW + Counter 累加，混合间隔 =N 与 >N，每轮对齐比较终值）；
  *   2. Demo6DeepchainStaged（N=4）混合间隔多轮 + 每轮 outValid 单拍脉冲宽度断言
  *      （valid 链为 RegNext 纯延迟线 ⇒ 末级恰好一拍高，不得锁高/展宽）；
  *   3. Demo2MatchStaged 命中/未命中交替的稳态正确性与输出稳定性（无中间态写）。
  *
  * 时序契约：io.valid 单拍脉冲发起调用，相邻脉冲间隔 ≥ N。
  */
class StagedEquivalenceExtraSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  private val Mask16 = (BigInt(1) << 16) - 1

  /** 确定性伪随机（LCG），避免测试间相互依赖 */
  private def lcg(seed: Long): Iterator[Int] = new Iterator[Int] {
    private var s = seed
    override def hasNext: Boolean = true
    override def next(): Int = {
      s = s * 6364136223846793005L + 1442695040888963407L
      ((s >>> 33) & 0xffff).toInt
    }
  }

  behavior.of("Demo4ExternStagedIngress vs Demo4ExternIngress（N=1 基线，多轮混合间隔等价）")

  it should "6 轮连续脉冲（间隔 4/5/4/6/4/7）RMW+Counter 累加与 N=1 基线逐轮一致" in {
    val rounds = 6
    val intervals = Seq(4, 5, 4, 6, 4, 7) // 均 ≥ N=4（E1 加权下 demo4 W=3 → n=4），含恰好 =N 与 >N
    val baselineStats = scala.collection.mutable.ArrayBuffer.empty[BigInt]
    val baselineHits = scala.collection.mutable.ArrayBuffer.empty[BigInt]

    // N=1 基线：每轮 valid 高 1 拍，随后 (interval-1) 拍低
    test(new Demo4ExternIngress) { c =>
      c.io.valid.poke(false.B)
      c.clock.step(1)
      (0 until rounds).foreach { r =>
        c.io.valid.poke(true.B)
        c.clock.step(1)
        c.io.valid.poke(false.B)
        baselineStats += c.io.ex_stats(3).peek().litValue
        baselineHits += c.io.ex_hits(3).peek().litValue
        c.clock.step(intervals(r) - 1)
      }
    }

    // N=4 切拍：单拍脉冲，间隔 = intervals(r)；脉冲后第 4 拍（outValid 拍后一拍）状态可见
    test(new Demo4ExternStagedIngress) { c =>
      c.io.valid.poke(false.B)
      c.clock.step(1)
      (0 until rounds).foreach { r =>
        c.io.valid.poke(true.B)
        c.clock.step(1) // 第 1 拍
        c.io.valid.poke(false.B)
        c.io.outValid.expect(false.B)
        c.clock.step(1) // 第 2 拍
        c.io.outValid.expect(false.B)
        c.clock.step(1) // 第 3 拍：末级 sV_3
        c.io.outValid.expect(true.B)
        c.clock.step(1) // 第 4 拍：写已提交
        c.io.outValid.expect(false.B) // outValid 单拍脉冲（不锁高）
        c.io.ex_stats(3).expect(baselineStats(r).U(16.W))
        c.io.ex_hits(3).expect(baselineHits(r).U(32.W))
        c.clock.step(intervals(r) - 4) // 补齐发起间隔
        // 空闲期间无变化
        c.io.ex_stats(3).expect(baselineStats(r).U(16.W))
        c.io.ex_hits(3).expect(baselineHits(r).U(32.W))
        c.io.outValid.expect(false.B)
      }
    }
  }

  behavior.of("Demo6DeepchainStagedIngress（N=4，混合间隔多轮 + outValid 脉宽）")

  it should "6 轮混合间隔（4/6/5/8/4/7）每轮 outValid 恰好一拍、状态终值正确累加" in {
    val rounds = 6
    val intervals = Seq(4, 6, 5, 8, 4, 7) // 均 ≥ N=4
    val rnd = lcg(20240601L)
    // 每轮（输入向量, accIn, 加法链和）
    val roundsData: Seq[(Seq[BigInt], BigInt, BigInt)] =
      (0 until rounds).map { _ =>
        val vals = (0 until 16).map(_ => BigInt(rnd.next()))
        val accIn = BigInt(rnd.next())
        (vals, accIn, vals.foldLeft(BigInt(0))(_ + _) & Mask16)
      }

    test(new Demo6DeepchainStagedIngress) { c =>
      c.io.valid.poke(false.B)
      c.clock.step(1)
      roundsData.zipWithIndex.foreach { case ((vals, accIn, sum), r) =>
        pokeMeta(c, vals, accIn)
        c.io.valid.poke(true.B)
        c.clock.step(1) // 第 1 拍
        c.io.valid.poke(false.B)
        c.io.outValid.expect(false.B)
        c.clock.step(1) // 第 2 拍
        c.io.outValid.expect(false.B)
        c.clock.step(1) // 第 3 拍：末级 sV_3
        c.io.outValid.expect(true.B)
        c.io.metaOut.acc.expect(sum.U(16.W)) // 末级组合直出与 outValid 同拍
        c.clock.step(1) // 第 4 拍：写已提交
        c.io.outValid.expect(false.B) // 脉宽恒为 1 拍（RegNext 延迟线不自保持）
        c.io.ex_stats(0).expect(accIn.U(16.W))
        c.io.ex_hits(0).expect((r + 1).U(32.W))
        c.clock.step(intervals(r) - 4) // 补齐发起间隔
        c.io.ex_stats(0).expect(accIn.U(16.W))
        c.io.ex_hits(0).expect((r + 1).U(32.W))
        c.io.outValid.expect(false.B)
      }
    }
  }

  private def pokeMeta(c: Demo6DeepchainStagedIngress, vals: Seq[BigInt], accIn: BigInt): Unit = {
    c.io.metaIn.elements.foreach { case (name, data) =>
      if (name.startsWith("f") && name.drop(1).forall(_.isDigit))
        data.asInstanceOf[UInt].poke(vals(name.drop(1).toInt).U(16.W))
    }
    c.io.metaIn.acc.asInstanceOf[UInt].poke(accIn.U(16.W))
  }

  behavior.of("Demo2MatchStagedIngress（E1 加权下 n=2，命中/未命中交替稳态）")

  it should "命中→未命中→命中交替：每次切换后稳态输出正确，输入保持期间输出不抖动" in {
    test(new Demo2MatchStagedIngress) { c =>
      def hit0800(np: Int, baseCls: Int): Unit = {
        c.io.hdrIn.ethernet.etherType.poke(0x0800.U(16.W))
        c.io.metaIn.normPort.poke(np.U(16.W))
        c.io.metaIn.cls.poke(baseCls.U(8.W))
        c.clock.step(2) // 流水深度 n=3 → 2 拍后稳态
        c.io.metaOut.cls.expect(((7 + (np & 0xff)) & 0xff).U(8.W))
        c.io.metaOut.normPort.expect(0.U(16.W))
      }
      def miss(cls: Int, np: Int): Unit = {
        c.io.hdrIn.ethernet.etherType.poke(0x0806.U(16.W))
        c.io.metaIn.normPort.poke(np.U(16.W))
        c.io.metaIn.cls.poke(cls.U(8.W))
        c.clock.step(2)
        c.io.metaOut.cls.expect(cls.U(8.W))
        c.io.metaOut.normPort.expect(np.U(16.W))
      }
      hit0800(0x0203, 0)
      miss(0x5a, 0x1234)
      hit0800(0x00ff, 0x11)
      miss(0x77, 0xbeef)
      // 0x86dd 表项
      c.io.hdrIn.ethernet.etherType.poke(0x86dd.U(16.W))
      c.io.metaIn.normPort.poke(0x00ff.U(16.W))
      c.io.metaIn.cls.poke(0.U(8.W))
      c.clock.step(2)
      c.io.metaOut.cls.expect(((9 + 0xff) & 0xff).U(8.W))
      c.io.metaOut.normPort.expect(0.U(16.W))
      // 输入保持 8 拍（> N）：输出稳定不变（无虚假翻转）
      c.clock.step(8)
      c.io.metaOut.cls.expect(((9 + 0xff) & 0xff).U(8.W))
      c.io.metaOut.normPort.expect(0.U(16.W))
      // header 透传不受切拍影响
      c.io.hdrIn.ethernet.etherType.poke(0x86dd.U(16.W)) // 保持
      c.io.hdrOut.ethernet.etherType.expect(0x86dd.U(16.W))
    }
  }
}
