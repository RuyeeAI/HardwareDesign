package FPP.Parser

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ParserCore functional tests.
 *
 * Byte convention used by [[ParserCore]]: byte `i` of the packet occupies bits
 * `[511-8*i : 504-8*i]` of the 512-bit input vector, i.e. the first byte of the
 * packet sits in the MSBs (network order).
 *
 * The DUT is a plain clocked module: one `step()` advances one FSM state, so a
 * complete parse takes as many cycles as there are headers plus a couple of
 * housekeeping states.
 */
class ParserCoreSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ---------------------------------------------------------------- helpers

  /**
   * Build a 512-bit literal from a byte sequence. Network (MSB-first) order:
   * byte 0 lands in bits [511:504], byte k in bits [511-8k : 504-8k].
   */
  private def pktBytes(bs: Seq[Int]): BigInt =
    bs.zipWithIndex.map { case (b, i) => BigInt(b & 0xff) << (504 - 8 * i) }
      .foldLeft(BigInt(0))(_ | _)

  /** Zero-pad a byte sequence up to 64 bytes (the DUT window is 512 bit). */
  private def pkt(bs: Seq[Int]): BigInt = {
    require(bs.length <= 64, s"packet too long for the 512-bit window: ${bs.length} bytes")
    pktBytes(bs ++ Seq.fill(64 - bs.length)(0))
  }

  /** Ones-complement sum over 16-bit big-endian words (IPv4 header checksum). */
  private def ipChecksum(hdr: Seq[Int]): Int = {
    require(hdr.length % 2 == 0)
    var sum = 0
    for (i <- hdr.indices by 2) sum += ((hdr(i) & 0xff) << 8) | (hdr(i + 1) & 0xff)
    while ((sum >> 16) != 0) sum = (sum & 0xffff) + (sum >> 16)
    (~sum) & 0xffff
  }

  private def be16(v: Int): Seq[Int] = Seq((v >> 8) & 0xff, v & 0xff)
  private def be32(v: Long): Seq[Int] =
    Seq(((v >> 24) & 0xff).toInt, ((v >> 16) & 0xff).toInt, ((v >> 8) & 0xff).toInt, (v & 0xff).toInt)

  /** Ethernet + IPv4(20B, no options) + TCP(20B) */
  private def ethIpTcp(proto: Int, l4: Seq[Int], ttl: Int = 0x40): Seq[Int] = {
    val l3len = 20 + l4.length
    val ipHdr = Seq(
      0x45, 0x00
    ) ++ be16(l3len) ++ be16(0) ++ be16(0) ++ Seq(ttl, proto) ++ be16(0) ++
      be32(0x0a000001L) ++ be32(0x0a000002L)
    val csum = ipChecksum(ipHdr)
    val ip = ipHdr.take(10) ++ be16(csum) ++ ipHdr.drop(12)
    Seq(0x00, 0x11, 0x22, 0x33, 0x44, 0x55, // DA
        0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, // SA
        0x08, 0x00) ++ ip ++ l4            // EtherType + IPv4 + L4
  }

  private def tcpHdr(dstPort: Int = 80): Seq[Int] =
    be16(1234) ++ be16(dstPort) ++ be32(1L) ++ be32(2L) ++
      Seq(0x50, 0x02) ++ be16(65535) ++ be16(0) ++ be16(0)

  private def udpHdr(dstPort: Int, len: Int): Seq[Int] =
    be16(1234) ++ be16(dstPort) ++ be16(len) ++ be16(0)

  /** Run the parser until `parseDone` asserts; returns the cycle count. */
  private def runToDone(dut: ParserCore, maxCycles: Int = 64): Int = {
    dut.io.in.valid.poke(false.B)
    var n = 0
    while (!dut.io.parseDone.peek().litToBoolean && n < maxCycles) {
      dut.clock.step(1)
      n += 1
    }
    n
  }

  private def feed(dut: ParserCore, bytes: BigInt): Unit = {
    dut.io.in.bits.poke(bytes.U(512.W))
    dut.io.in.valid.poke(true.B)
    dut.clock.step(1)
    dut.io.in.valid.poke(false.B)
  }

  // ---------------------------------------------------------------- tests

  behavior of "ParserCore elaboration"

  it should "elaborate with the default (unpipelined) configuration" in {
    test(new ParserCore(ParserPipelineConfig.default)) { _ => () }
  }

  it should "elaborate with the mildTiming configuration" in {
    test(new ParserCore(ParserPipelineConfig.mildTiming)) { _ => () }
  }

  it should "elaborate with the aggressiveTiming configuration" in {
    test(new ParserCore(ParserPipelineConfig.aggressiveTiming)) { _ => () }
  }

  behavior of "ParserCore L2/L3/L4 parsing"

  it should "parse Ethernet + IPv4 + TCP and report 3 headers" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      feed(dut, pkt(ethIpTcp(6, tcpHdr())))
      val cycles = runToDone(dut)
      dut.io.parseDone.peek().litToBoolean shouldBe true
      dut.io.out.valid.peek().litToBoolean shouldBe true
      dut.io.out.bits.valid.peek().litToBoolean shouldBe true
      dut.io.meta.parseError.peek().litToBoolean shouldBe false
      dut.io.out.bits.headerCount.peek().litValue shouldBe 3
      info(s"parse took $cycles cycles")
    }
  }

  it should "record PHO offsets for each parsed header (Eth=0, IPv4=14, TCP=34)" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      feed(dut, pkt(ethIpTcp(6, tcpHdr())))
      runToDone(dut)
      val pho = (0 until 3).map(i => dut.io.out.bits.pho(i).peek().litValue)
      pho shouldBe Seq(0, 14, 34)
      val types = (0 until 3).map(i => dut.io.out.bits.phi(i).headerType.peek().litValue)
      types shouldBe Seq(HeaderType.ETH.litValue, HeaderType.IPV4.litValue, HeaderType.TCP.litValue)
    }
  }

  it should "parse Ethernet + IPv4 + UDP and stop at UDP when the port is not a tunnel" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      feed(dut, pkt(ethIpTcp(17, udpHdr(53, 8))))
      runToDone(dut)
      dut.io.out.bits.valid.peek().litToBoolean shouldBe true
      dut.io.out.bits.headerCount.peek().litValue shouldBe 3
      dut.io.out.bits.phi(2).headerType.peek().litValue shouldBe HeaderType.UDP.litValue
    }
  }

  it should "detect an IPv4 version error" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      val bytes = ethIpTcp(6, tcpHdr())
      val bad = bytes.updated(14, 0x65) // version 6, ihl 5
      feed(dut, pkt(bad))
      runToDone(dut)
      dut.io.meta.parseError.peek().litToBoolean shouldBe true
      dut.io.meta.errorInfo.peek().litValue shouldBe HeaderErrorCode.Ipv4VersionError.litValue
    }
  }

  it should "detect a zero IPv4 TTL" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      feed(dut, pkt(ethIpTcp(6, tcpHdr(), ttl = 0)))
      runToDone(dut)
      dut.io.meta.parseError.peek().litToBoolean shouldBe true
      dut.io.meta.errorInfo.peek().litValue shouldBe HeaderErrorCode.Ipv4TtlZero.litValue
    }
  }

  behavior of "ParserCore VLAN / MPLS"

  it should "parse a single VLAN tag before IPv4" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      val tag = Seq(0x81, 0x00, 0x00, 0x64, 0x08, 0x00) // TPID + TCI + IPv4
      val bytes = ethIpTcp(6, tcpHdr()).take(12) ++ tag ++ ethIpTcp(6, tcpHdr()).drop(14)
      feed(dut, pkt(bytes))
      runToDone(dut)
      dut.io.out.bits.valid.peek().litToBoolean shouldBe true
      dut.io.meta.vlanCount.peek().litValue shouldBe 1
      dut.io.out.bits.headerCount.peek().litValue shouldBe 4
      dut.io.out.bits.phi(1).headerType.peek().litValue shouldBe HeaderType.VLAN.litValue
      dut.io.out.bits.pho(1).peek().litValue shouldBe 12
      dut.io.out.bits.pho(2).peek().litValue shouldBe 18
      dut.io.out.bits.pho(3).peek().litValue shouldBe 38
    }
  }

  it should "parse QinQ (two VLAN tags) before IPv4" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      val tag = Seq(0x88, 0xa8, 0x00, 0x0a, 0x81, 0x00, 0x00, 0x64, 0x08, 0x00)
      val bytes = ethIpTcp(6, tcpHdr()).take(12) ++ tag ++ ethIpTcp(6, tcpHdr()).drop(14)
      feed(dut, pkt(bytes))
      runToDone(dut)
      dut.io.meta.vlanCount.peek().litValue shouldBe 2
      dut.io.out.bits.headerCount.peek().litValue shouldBe 5
    }
  }

  it should "parse an MPLS label stack and continue at IPv4" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      // 线序: byte0=Label[19:12] byte1=Label[11:4] byte2=Label[3:0]|TC|S byte3=TTL
      // label 100 = 0x00064 -> 00 06 40(S=0) / ttl 0x40
      // label 200 = 0x000C8 -> 00 0c 81(S=1) / ttl 0x40
      val l1 = Seq(0x00, 0x06, 0x40, 0x40)
      val l2 = Seq(0x00, 0x0c, 0x81, 0x40)
      val bytes = ethIpTcp(6, tcpHdr()).take(12) ++ Seq(0x88, 0x47) ++ l1 ++ l2 ++
        ethIpTcp(6, tcpHdr()).drop(14)
      feed(dut, pkt(bytes))
      runToDone(dut)
      dut.io.out.bits.valid.peek().litToBoolean shouldBe true
      dut.io.meta.mplsCount.peek().litValue shouldBe 2
      dut.io.out.bits.phi(1).headerType.peek().litValue shouldBe HeaderType.MPLS.litValue
      dut.io.out.bits.phi(2).headerType.peek().litValue shouldBe HeaderType.MPLS.litValue
      dut.io.out.bits.phi(3).headerType.peek().litValue shouldBe HeaderType.IPV4.litValue
    }
  }

  behavior of "ParserCore tunnels"

  it should "dispatch UDP dstPort 4789 into the VXLAN state" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      // 64-byte window: Eth(14) + IPv4(20) + UDP(8) + VXLAN(8) + inner Eth(14) = 64.
      // The inner IPv4 would start at byte 64 and is therefore all-zero -> expected error.
      val innerEth =
        Seq(0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0x08, 0x00)
      val vxlan = Seq(0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x64, 0x00) // flags + VNI
      val udp = udpHdr(4789, 8 + innerEth.length)
      val bytes = ethIpTcp(17, udp).take(34) ++ udp ++ vxlan ++ innerEth
      feed(dut, pkt(bytes))
      runToDone(dut)
      val types = (0 until 5).map(i => dut.io.out.bits.phi(i).headerType.peek().litValue)
      types shouldBe Seq(
        HeaderType.ETH.litValue,
        HeaderType.IPV4.litValue,
        HeaderType.UDP.litValue,
        HeaderType.VXLAN.litValue,
        HeaderType.ETH.litValue
      )
      val pho = (0 until 5).map(i => dut.io.out.bits.pho(i).peek().litValue)
      pho shouldBe Seq(0, 14, 34, 42, 50)
      // inner IPv4 starts at byte 64 -> outside the window -> version error
      dut.io.meta.parseError.peek().litToBoolean shouldBe true
      dut.io.meta.errorInfo.peek().litValue shouldBe HeaderErrorCode.Ipv4VersionError.litValue
    }
  }

  behavior of "ParserCore pipeline registers"

  /** Returns (headerCount, pho, phiTypes, valid, parseError) observed at parseDone. */
  private def observe(dut: ParserCore): (BigInt, Seq[BigInt], Seq[BigInt], Boolean, Boolean) = {
    runToDone(dut)
    dut.io.parseDone.peek().litToBoolean shouldBe true
    val hc = dut.io.out.bits.headerCount.peek().litValue
    val n = hc.toInt
    (
      hc,
      (0 until n).map(i => dut.io.out.bits.pho(i).peek().litValue),
      (0 until n).map(i => dut.io.out.bits.phi(i).headerType.peek().litValue),
      dut.io.out.bits.valid.peek().litToBoolean,
      dut.io.meta.parseError.peek().litToBoolean
    )
  }

  for ((name, cfg) <- Seq(
         "default" -> ParserPipelineConfig.default,
         "mildTiming" -> ParserPipelineConfig.mildTiming,
         "aggressiveTiming" -> ParserPipelineConfig.aggressiveTiming
       )) {
    it should s"parse Ethernet + IPv4 + TCP identically with the $name pipeline" in {
      test(new ParserCore(cfg)) { dut =>
        feed(dut, pkt(ethIpTcp(6, tcpHdr())))
        val (hc, pho, types, valid, err) = observe(dut)
        err shouldBe false
        valid shouldBe true
        hc shouldBe 3
        pho shouldBe Seq(0, 14, 34)
        types shouldBe Seq(
          HeaderType.ETH.litValue,
          HeaderType.IPV4.litValue,
          HeaderType.TCP.litValue
        )
      }
    }

    it should s"record exactly one PHO entry per header with the $name pipeline (QinQ)" in {
      test(new ParserCore(cfg)) { dut =>
        val tag = Seq(0x88, 0xa8, 0x00, 0x0a, 0x81, 0x00, 0x00, 0x64, 0x08, 0x00)
        val base = ethIpTcp(6, tcpHdr())
        feed(dut, pkt(base.take(12) ++ tag ++ base.drop(14)))
        val (hc, pho, _, _, err) = observe(dut)
        err shouldBe false
        hc shouldBe 5
        pho shouldBe Seq(0, 12, 16, 22, 42)
        dut.io.meta.vlanCount.peek().litValue shouldBe 2
      }
    }
  }

  behavior of "ParserCore readiness"

  it should "accept a second packet after the first completes" in {
    test(new ParserCore(ParserPipelineConfig.default)) { dut =>
      feed(dut, pkt(ethIpTcp(6, tcpHdr())))
      runToDone(dut)
      dut.clock.step(1)
      feed(dut, pkt(ethIpTcp(6, tcpHdr())))
      runToDone(dut)
      dut.io.out.bits.headerCount.peek().litValue shouldBe 3
      dut.io.out.bits.pho(1).peek().litValue shouldBe 14
    }
  }
}
