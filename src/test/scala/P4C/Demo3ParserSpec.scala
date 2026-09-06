package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** M3 demo 端到端测试：demo3-parser.p4 → Demo3ParserTopParser（FSM，512-bit 网络序窗口）。 */
class Demo3ParserSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  /** 报文字节 → 512-bit 窗口值：byte i 占 bits [511-8i, 504-8i]（网络序，首字节在最高位）。 */
  private def window(bytes: Seq[Int]): BigInt =
    (bytes.foldLeft(BigInt(0))((acc, b) => (acc << 8) | (b & 0xff))) << (512 - 8 * bytes.length)

  private def ethBytes(dst: BigInt, src: BigInt, etherType: Int): Seq[Int] = {
    def f(v: BigInt, n: Int) = (0 until n).map(i => ((v >> (8 * (n - 1 - i))) & 0xff).toInt)
    f(dst, 6) ++ f(src, 6) ++ Seq((etherType >> 8) & 0xff, etherType & 0xff)
  }

  private def ipv4Bytes(
    version: Int = 4, ihl: Int = 5, diffserv: Int = 0, totalLen: Int = 0x0032,
    ident: Int = 0x1234, flagsFrag: Int = 0x4000, ttl: Int = 64, proto: Int = 17,
    checksum: Int = 0, src: BigInt = 0x0a000001, dst: BigInt = 0x0a000002,
  ): Seq[Int] = {
    def f(v: BigInt, n: Int) = (0 until n).map(i => ((v >> (8 * (n - 1 - i))) & 0xff).toInt)
    Seq((version << 4) | ihl, diffserv) ++ f(totalLen, 2) ++ f(ident, 2) ++
      f(flagsFrag, 2) ++ Seq(ttl, proto) ++ f(checksum, 2) ++ f(src, 4) ++ f(dst, 4)
  }

  behavior.of("Demo3ParserTopParser（由 demo3-parser.p4 生成）")

  it should "解析 Ethernet + IPv4（0x0800 → parse_ipv4 → accept）" in {
    test(new Demo3ParserTopParser) { c =>
      val dst = BigInt("ffffffffffff", 16)
      val src = BigInt("001122334455", 16)
      val bytes = ethBytes(dst, src, 0x0800) ++ ipv4Bytes()
      c.io.in.poke(window(bytes).U(512.W))
      c.io.done.expect(false.B)
      c.clock.step(4)
      c.io.done.expect(true.B)
      c.io.error.expect(false.B)

      c.io.hdrOut.ethernetValid.expect(true.B)
      c.io.hdrOut.ethernet.dstAddr.expect(dst.U(48.W))
      c.io.hdrOut.ethernet.srcAddr.expect(src.U(48.W))
      c.io.hdrOut.ethernet.etherType.expect(0x0800.U(16.W))

      c.io.hdrOut.ipv4Valid.expect(true.B)
      c.io.hdrOut.ipv4.version.expect(4.U(4.W))
      c.io.hdrOut.ipv4.ihl.expect(5.U(4.W))
      c.io.hdrOut.ipv4.totalLen.expect(0x0032.U(16.W))
      c.io.hdrOut.ipv4.identification.expect(0x1234.U(16.W))
      c.io.hdrOut.ipv4.flags.expect(0x2.U(3.W)) // 0x4000: flags=010, fragOffset=0
      c.io.hdrOut.ipv4.fragOffset.expect(0.U(13.W))
      c.io.hdrOut.ipv4.ttl.expect(64.U(8.W))
      c.io.hdrOut.ipv4.protocol.expect(17.U(8.W))
      c.io.hdrOut.ipv4.srcAddr.expect(0x0a000001L.U(32.W))
      c.io.hdrOut.ipv4.dstAddr.expect(0x0a000002L.U(32.W))
    }
  }

  it should "非 IPv4 以太类型走 default → accept，只解析以太头" in {
    test(new Demo3ParserTopParser) { c =>
      val dst = BigInt("0180c2000000", 16)
      val src = BigInt("001122334455", 16)
      c.io.in.poke(window(ethBytes(dst, src, 0x86dd) ++ Seq.fill(20)(0xab)).U(512.W))
      c.clock.step(4)
      c.io.done.expect(true.B)
      c.io.error.expect(false.B)
      c.io.hdrOut.ethernetValid.expect(true.B)
      c.io.hdrOut.ethernet.etherType.expect(0x86dd.U(16.W))
      c.io.hdrOut.ipv4Valid.expect(false.B)
    }
  }
}
