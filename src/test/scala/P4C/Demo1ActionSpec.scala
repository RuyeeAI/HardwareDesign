package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** M1 demo 端到端测试：demo1-action.p4 → Demo1ActionIngress（组合逻辑）。 */
class Demo1ActionSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  behavior.of("Demo1ActionIngress（由 demo1-action.p4 生成）")

  it should "apply rewrite 后按 P4 语义更新字段，其余字段透传" in {
    test(new Demo1ActionIngress) { c =>
      val dst = BigInt("112233445566", 16)
      val src = BigInt("aabbccddeeff", 16)
      c.io.hdrIn.ethernet.dstAddr.poke(dst.U(48.W))
      c.io.hdrIn.ethernet.srcAddr.poke(src.U(48.W))
      c.io.hdrIn.ethernet.etherType.poke(0x1234.U(16.W))
      c.io.metaIn.normPort.poke(0.U(16.W))
      c.io.metaIn.cls.poke(0.U(8.W))

      // etherType = 0x0800 ^ 0x00ff = 0x08ff
      c.io.hdrOut.ethernet.etherType.expect(0x08ff.U(16.W))
      // normPort = (srcAddr[15:0] + 1) << 1（16 位模）
      val srcLow16 = src & 0xffff
      val expectNorm = ((srcLow16 + 1) << 1) & 0xffff
      c.io.metaOut.normPort.expect(expectNorm.U(16.W))
      // cls = 8w3
      c.io.metaOut.cls.expect(3.U(8.W))
      // 未写的字段透传
      c.io.hdrOut.ethernet.dstAddr.expect(dst.U(48.W))
      c.io.hdrOut.ethernet.srcAddr.expect(src.U(48.W))
    }
  }

  it should "算术回绕与移位截断符合 16 位模语义" in {
    test(new Demo1ActionIngress) { c =>
      val srcLow = 0xffff // 16 位加法回绕：0xffff+1 = 0，<<1 后仍为 0
      c.io.hdrIn.ethernet.srcAddr.poke((BigInt("aabbccdd", 16) << 16 | srcLow).U(48.W))
      c.io.hdrIn.ethernet.dstAddr.poke(0.U(48.W))
      c.io.hdrIn.ethernet.etherType.poke(0.U(16.W))
      c.io.metaIn.normPort.poke(0.U(16.W))
      c.io.metaIn.cls.poke(0.U(8.W))
      c.io.metaOut.normPort.expect(0.U(16.W))
      c.io.metaOut.cls.expect(3.U(8.W))
    }
  }
}
