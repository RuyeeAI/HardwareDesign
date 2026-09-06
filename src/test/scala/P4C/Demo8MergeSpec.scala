package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** X3：跨 DAG 同值边界寄存器合并（demo8-merge.p4 → Demo8MergeIngress）。
  *
  * 两条文本相同的语句在切拍下产生两组同文本跨级节点——合并后 RegEnable 由 4 个
  * 减为 2 个；行为与未合并语义一致（同值同拍使能，共享无副作用）。
  */
class Demo8MergeSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  private val genFile = osPath("generated/p4c/Demo8Merge.scala")

  private def osPath(rel: String): String =
    java.nio.file.Paths.get(rel).toAbsolutePath.toString

  behavior.of("Demo8MergeIngress（由 demo8-merge.p4 生成）")

  it should "行为不变：acc = (a+b)+(c+d)（stages=2 两拍后末级输出）" in {
    test(new Demo8MergeIngress) { c =>
      c.io.metaIn.a.poke(1.U(16.W))
      c.io.metaIn.b.poke(2.U(16.W))
      c.io.metaIn.c.poke(3.U(16.W))
      c.io.metaIn.d.poke(4.U(16.W))
      c.io.valid.poke(true.B)
      c.clock.step(2) // 末级（sV_1）拍：合并后的边界寄存器值正确求和
      c.io.metaOut.acc.expect(10.U(16.W))
      c.clock.step(1) // 重复发起（同值）→ 结果稳定
      c.io.metaOut.acc.expect(10.U(16.W))
    }
  }

  it should "同值边界寄存器被合并：RegEnable 4 → 2，且无重复 val 定义" in {
    val code = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(genFile)),
      java.nio.charset.StandardCharsets.UTF_8)
    "RegEnable\\(".r.findAllMatchIn(code).size should be(2)
    "val v_0_0 =".r.findAllMatchIn(code).size should be(1)
    "val v_0_1 =".r.findAllMatchIn(code).size should be(1)
  }
}
