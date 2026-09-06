package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** M4 demo 端到端测试：demo4-extern.p4 → Demo4ExternIngress（Register/Counter 状态单元）。 */
class Demo4ExternSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  behavior.of("Demo4ExternIngress（由 demo4-extern.p4 生成）")

  it should "valid 时执行 read-modify-write 与 count，valid=0 时不写" in {
    test(new Demo4ExternIngress) { c =>
      c.io.valid.poke(true.B)
      c.clock.step(1)
      c.io.ex_stats(3).expect(1.U(16.W))
      c.io.ex_hits(3).expect(1.U(32.W))
      c.clock.step(1)
      c.io.ex_stats(3).expect(2.U(16.W)) // read-modify-write 连续两拍各 +1
      c.io.ex_hits(3).expect(2.U(32.W))

      // valid 拉低：状态保持
      c.io.valid.poke(false.B)
      c.clock.step(1)
      c.clock.step(1)
      c.io.ex_stats(3).expect(2.U(16.W))
      c.io.ex_hits(3).expect(2.U(32.W))
      // 其他表项不受影响
      c.io.ex_stats(0).expect(0.U(16.W))
      c.io.ex_hits(7).expect(0.U(32.W))
    }
  }

  it should "组合透传 header（与状态无关）" in {
    test(new Demo4ExternIngress) { c =>
      c.io.valid.poke(false.B)
      c.io.hdrIn.ethernet.srcAddr.poke(BigInt("aabbccddeeff", 16).U(48.W))
      c.io.hdrIn.ethernet.dstAddr.poke(BigInt(0x1234).U(48.W))
      c.io.hdrIn.ethernet.etherType.poke(0x0800.U(16.W))
      c.io.hdrOut.ethernet.srcAddr.expect(BigInt("aabbccddeeff", 16).U(48.W))
      c.io.hdrOut.ethernet.dstAddr.expect(BigInt(0x1234).U(48.W))
      c.io.hdrOut.ethernet.etherType.expect(0x0800.U(16.W))
    }
  }
}
