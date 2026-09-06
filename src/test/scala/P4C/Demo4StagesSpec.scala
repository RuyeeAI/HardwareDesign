package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** demo4-extern / demo2-match 切拍变体等价性测试（P1）：
  *   - Demo4ExternStagedIngress：Register read-modify-write + Counter 的切拍等价、
  *     valid 脉冲门控（发起间隔 ≥ N）、outValid 末级时序；
  *   - Demo2MatchStagedIngress：表匹配原子（key/hit 第 0 级）、action 部分切拍、
  *     hit 与 action 数据在末级同步。
  *
  * 时序契约：io.valid 为单拍脉冲（每个脉冲发起一次调用），流水 N 级，末级 stageValid
  * （io.outValid）拍提交一次状态写；相邻脉冲间隔 ≥ N。
  *
  * E1 注：demo4 的 DAG 加权深度 W = 3（RegRead 权重 2 + Bin 权重 1），预算
  * P4C_STAGED_STAGES=4 下实际级数 n = min(4, W+1) = 4（旧深度公式为 D=2 → n=3，
  * 本文件各时序断言已按加权模型同步更新）。
  */
class Demo4StagesSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  behavior.of("Demo4ExternStagedIngress（demo4-extern 切拍变体，N=4）")

  it should "valid 脉冲发起一次 read-modify-write + count，终值与 N=1 等价" in {
    test(new Demo4ExternStagedIngress) { c =>
      c.io.valid.poke(true.B) // 脉冲（第 0 拍）
      c.clock.step(1)
      c.io.valid.poke(false.B)
      c.io.outValid.expect(false.B) // 第 1 拍
      c.clock.step(1)
      c.io.outValid.expect(false.B) // 第 2 拍
      c.clock.step(1)
      c.io.outValid.expect(true.B) // 第 3 拍（末级 sV_3）
      c.io.ex_stats(3).expect(0.U(16.W)) // 状态尚未提交
      c.clock.step(1)
      c.io.outValid.expect(false.B) // 单拍脉冲
      c.io.ex_stats(3).expect(1.U(16.W))
      c.io.ex_hits(3).expect(1.U(32.W))
      // 第二次调用：相邻脉冲间隔 4 拍（= N）
      c.io.valid.poke(true.B)
      c.clock.step(1)
      c.io.valid.poke(false.B)
      c.clock.step(3) // 第 7 拍末提交，第 8 拍可见
      c.io.ex_stats(3).expect(2.U(16.W))
      c.io.ex_hits(3).expect(2.U(32.W))
    }
  }

  it should "valid 低电平期间无写、outValid 保持低" in {
    test(new Demo4ExternStagedIngress) { c =>
      c.io.valid.poke(true.B)
      c.clock.step(1)
      c.io.valid.poke(false.B)
      c.clock.step(3) // 第 3 拍末提交，第 4 拍可见
      c.io.ex_stats(3).expect(1.U(16.W))
      c.io.ex_hits(3).expect(1.U(32.W))
      // valid=0 持续多拍（≥ N）：无任何写
      c.clock.step(4)
      c.io.ex_stats(3).expect(1.U(16.W))
      c.io.ex_hits(3).expect(1.U(32.W))
      c.io.outValid.expect(false.B)
    }
  }

  behavior.of("Demo2MatchStagedIngress（demo2-match 切拍变体，E1 加权下 n=2，表匹配原子 + action 切拍）")

  /** staged 变体 action：meta.cls = c + meta.normPort[7:0]; meta.normPort = 16w0 */
  private def expectStagedCls(c: Demo2MatchStagedIngress, entryConst: Int, normPort: Int): Unit = {
    c.io.metaOut.cls.expect(((entryConst + (normPort & 0xff)) & 0xff).U(8.W))
    c.io.metaOut.normPort.expect(0.U(16.W))
  }

  it should "命中/未命中各表项输出正确，hit 与 action 数据在末级同步" in {
    test(new Demo2MatchStagedIngress) { c =>
      c.io.valid.poke(true.B)
      // 表项 0x0800：set_cls(8w7)
      c.io.hdrIn.ethernet.etherType.poke(0x0800.U(16.W))
      c.io.metaIn.normPort.poke(0x0203.U(16.W))
      c.io.metaIn.cls.poke(0.U(8.W))
      c.clock.step(2) // 流水填满（E1 加权下 n=2 → 1 拍后末级稳定，step 2 充分）
      expectStagedCls(c, 7, 0x0203)
      // 切表项 0x86dd：set_cls(8w9)——key 变化后 hit 与 action 数据保持同步
      c.io.hdrIn.ethernet.etherType.poke(0x86dd.U(16.W))
      c.io.metaIn.normPort.poke(0x00ff.U(16.W))
      c.clock.step(2)
      expectStagedCls(c, 9, 0x00ff)
      // 未命中：default nop() → 透传
      c.io.hdrIn.ethernet.etherType.poke(0x0806.U(16.W))
      c.io.metaIn.normPort.poke(0x1234.U(16.W))
      c.io.metaIn.cls.poke(0x5a.U(8.W))
      c.clock.step(2)
      c.io.metaOut.cls.expect(0x5a.U(8.W))
      c.io.metaOut.normPort.expect(0x1234.U(16.W))
    }
  }

  it should "输出连续有效（无状态表：组合级流水持续跟随输入）" in {
    test(new Demo2MatchStagedIngress) { c =>
      // staged 无状态表使用常真 valid 链：边界寄存器逐拍跟随，输出持续更新
      c.io.hdrIn.ethernet.etherType.poke(0x0800.U(16.W))
      c.io.metaIn.normPort.poke(0x0001.U(16.W))
      c.io.metaIn.cls.poke(0.U(8.W))
      c.clock.step(3)
      expectStagedCls(c, 7, 0x0001)
    }
  }
}
