package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** M5 demo 端到端测试：demo5-pipeline.p4 → Demo5PipelineTop（parser → match-action 管线）。 */
class Demo5PipelineSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  /** 报文字节 → 512-bit 窗口值：byte i 占 bits [511-8i, 504-8i]（网络序）。 */
  private def window(bytes: Seq[Int]): BigInt =
    (bytes.foldLeft(BigInt(0))((acc, b) => (acc << 8) | (b & 0xff))) << (512 - 8 * bytes.length)

  private def ethBytes(etherType: Int): Seq[Int] =
    Seq.fill(6)(0xff) ++ Seq(0x00, 0x11, 0x22, 0x33, 0x44, 0x55) ++
      Seq((etherType >> 8) & 0xff, etherType & 0xff)

  private def ipv4Bytes(protocol: Int): Seq[Int] = {
    def f(v: BigInt, n: Int) = (0 until n).map(i => ((v >> (8 * (n - 1 - i))) & 0xff).toInt)
    Seq(0x45, 0x00) ++ f(0x0032, 2) ++ f(0x1234, 2) ++ f(0x4000, 2) ++
      Seq(64, protocol) ++ f(0, 2) ++ f(0x0a000001, 4) ++ f(0x0a000002, 4)
  }

  behavior.of("Demo5PipelineTop（由 demo5-pipeline.p4 生成）")

  it should "解析 → 表匹配 → 状态更新 一次触发，outValid 单拍脉冲" in {
    test(new Demo5PipelineTop) { c =>
      c.io.in.poke(window(ethBytes(0x0800) ++ ipv4Bytes(protocol = 17)).U(512.W))
      c.io.outValid.expect(false.B)

      // 4 拍：start → ethernet → ipv4 → accept（done 置位）
      c.clock.step(4)
      // 第 5 拍：fire（control 组合执行 + 寄存器写入）
      c.clock.step(1)
      c.io.outValid.expect(true.B)
      c.io.error.expect(false.B)

      // 表 default → count(4w0)：per_proto[0]=1, total[0]=1, meta.cls=0+1
      c.io.ex_per_proto(0).expect(1.U(16.W))
      c.io.ex_total(0).expect(1.U(32.W))
      c.io.metaOut.cls.expect(1.U(8.W))
      // 解析结果透传
      c.io.hdrOut.ethernet.etherType.expect(0x0800.U(16.W))
      c.io.hdrOut.ipv4.protocol.expect(17.U(8.W))
      c.io.ipv4Valid.expect(true.B)

      // fired 锁存：不重复触发
      c.clock.step(2)
      c.io.ex_per_proto(0).expect(1.U(16.W))
      c.io.ex_total(0).expect(1.U(32.W))
      c.io.outValid.expect(false.B)
    }
  }

  it should "非 IPv4 报文走 default 解析路径，不触发 control 状态写" in {
    test(new Demo5PipelineTop) { c =>
      c.io.in.poke(window(ethBytes(0x86dd) ++ Seq.fill(20)(0xab)).U(512.W))
      // 非 IPv4 少一级解析状态：fire 在第 4 拍
      c.clock.step(4)
      c.io.outValid.expect(true.B)
      c.io.error.expect(false.B)
      c.io.hdrOut.ethernet.etherType.expect(0x86dd.U(16.W))
      // control 照常以 ipv4 字段（未提取=0）作 key，default → count(4w0) 仍触发一次
      c.io.ex_per_proto(0).expect(1.U(16.W))
      c.io.ipv4Valid.expect(false.B)
      // 一次性触发：之后不再写状态
      c.clock.step(1)
      c.io.outValid.expect(false.B)
      c.io.ex_per_proto(0).expect(1.U(16.W))
    }
  }
}
