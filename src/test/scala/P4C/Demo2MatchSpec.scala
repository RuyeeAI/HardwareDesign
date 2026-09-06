package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** M2 demo 端到端测试：demo2-match.p4 → Demo2MatchIngress（exact 表静态融合）。 */
class Demo2MatchSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  behavior.of("Demo2MatchIngress（由 demo2-match.p4 生成）")

  it should "命中 0x0800 → set_cls(7)" in {
    test(new Demo2MatchIngress) { c =>
      c.io.hdrIn.ethernet.etherType.poke(0x0800.U(16.W))
      c.io.hdrIn.ethernet.dstAddr.poke(0.U(48.W))
      c.io.hdrIn.ethernet.srcAddr.poke(0.U(48.W))
      c.io.metaIn.cls.poke(1.U(8.W))
      c.io.metaIn.normPort.poke(1234.U(16.W))
      c.io.metaOut.cls.expect(7.U(8.W))
      c.io.metaOut.normPort.expect(0.U(16.W))
    }
  }

  it should "命中 0x86dd → set_cls(9)" in {
    test(new Demo2MatchIngress) { c =>
      c.io.hdrIn.ethernet.etherType.poke(0x86dd.U(16.W))
      c.io.hdrIn.ethernet.dstAddr.poke(0.U(48.W))
      c.io.hdrIn.ethernet.srcAddr.poke(0.U(48.W))
      c.io.metaIn.cls.poke(1.U(8.W))
      c.io.metaIn.normPort.poke(1234.U(16.W))
      c.io.metaOut.cls.expect(9.U(8.W))
      c.io.metaOut.normPort.expect(0.U(16.W))
    }
  }

  it should "未命中走 default(nop) → 字段透传" in {
    test(new Demo2MatchIngress) { c =>
      c.io.hdrIn.ethernet.etherType.poke(0x1234.U(16.W))
      c.io.hdrIn.ethernet.dstAddr.poke(0.U(48.W))
      c.io.hdrIn.ethernet.srcAddr.poke(0.U(48.W))
      c.io.metaIn.cls.poke(5.U(8.W))
      c.io.metaIn.normPort.poke(777.U(16.W))
      c.io.metaOut.cls.expect(5.U(8.W))
      c.io.metaOut.normPort.expect(777.U(16.W))
    }
  }
}
