package BaseCbb.memory

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Memory 读通路契约（Sp/TpMemoryWrap3）
 *
 * 三条都是实测出来的、曾经不对的契约：
 *  1. **端到端读延时 = `mem.readLatency`**（= latency + CheckIn + CheckOut）。
 *     注意 `mem.latency` **不含** CheckIn/CheckOut，用它当读延时在默认配置下会少算 1 拍。
 *  2. **CheckIn 必须锁住读地址**：SP 只有一个 addr，读写共用，
 *     若只按 `we` 采样，纯读访问的地址不会被捕获，读任意地址都返回"最后一次写入地址"的数据。
 *  3. **`dfx.eccErrAddr` 报的是出错那次读的地址**：不能报"报错当拍正在发起的下一个读地址"。
 *
 * 探针手法：写 A、再写 B（B 是"最后写入地址"），然后**读 A**。这样若地址锁存不对，
 * 读回的是 B 的值 → 同时暴露错误，且不会被"输出一直在刷新 m(raddr)"骗到提前命中。
 */
class MemoryWrap3ReadPathSpec extends AnyFlatSpec with Matchers {

  private val VAL_A  = 0xdeadbeefL
  private val VAL_B  = 0x12345678L
  private val ADDR_A = 3
  private val ADDR_B = 9

  private def mk(name: String, fIn: Boolean, fOut: Boolean, cIn: Boolean, cOut: Boolean,
                 protect: MemoryProtectType.MemoryProtectType = MemoryProtectType.ProtNone) =
    Memory(name = name, dataType = UInt(32.W), depth = 16, protect = protect,
      flopIn = fIn, flopOut = fOut, CheckIn = cIn, CheckOut = cOut)

  /** 返回 (读 A 的延时, 读回来的值) */
  private def probeTp(m: Memory): (Int, BigInt) = {
    var lat = -1
    var got = BigInt(-1)
    simulate(new TpMemoryWrap3(m)) { c =>
      c.reset.poke(false.B); c.io.dfx.init.poke(false.B)
      c.io.dfx.injCorrEn.poke(false.B); c.io.dfx.injUerrEn.poke(false.B)
      c.io.lgc.we.poke(false.B); c.io.lgc.re.poke(false.B)
      c.io.lgc.waddr.poke(0.U); c.io.lgc.raddr.poke(0.U); c.io.lgc.wdata.poke(0.U)
      c.clock.step(4)
      c.io.lgc.we.poke(true.B); c.io.lgc.waddr.poke(ADDR_A.U); c.io.lgc.wdata.poke(VAL_A.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)
      c.io.lgc.we.poke(true.B); c.io.lgc.waddr.poke(ADDR_B.U); c.io.lgc.wdata.poke(VAL_B.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B)
      c.io.lgc.raddr.poke(ADDR_B.U)          // 空闲期挂在最后写入的地址上
      c.clock.step(8)
      c.io.lgc.re.poke(true.B); c.io.lgc.raddr.poke(ADDR_A.U)
      var k = 0
      while (lat < 0 && k < 16) {
        c.clock.step(1); k += 1
        if (c.io.lgc.rdata.peek().litValue == BigInt(VAL_A)) lat = k
      }
      got = c.io.lgc.rdata.peek().litValue
    }
    (lat, got)
  }

  private def probeSp(m: Memory): (Int, BigInt) = {
    var lat = -1
    var got = BigInt(-1)
    simulate(new SpMemoryWrap3(m)) { c =>
      c.reset.poke(false.B); c.io.dfx.init.poke(false.B)
      c.io.dfx.injCorrEn.poke(false.B); c.io.dfx.injUerrEn.poke(false.B)
      c.io.lgc.we.poke(false.B); c.io.lgc.re.poke(false.B)
      c.io.lgc.addr.poke(0.U); c.io.lgc.wdata.poke(0.U)
      c.clock.step(4)
      c.io.lgc.we.poke(true.B); c.io.lgc.addr.poke(ADDR_A.U); c.io.lgc.wdata.poke(VAL_A.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)
      c.io.lgc.we.poke(true.B); c.io.lgc.addr.poke(ADDR_B.U); c.io.lgc.wdata.poke(VAL_B.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B)
      c.io.lgc.addr.poke(ADDR_B.U)
      c.clock.step(8)
      c.io.lgc.re.poke(true.B); c.io.lgc.addr.poke(ADDR_A.U)
      var k = 0
      while (lat < 0 && k < 16) {
        c.clock.step(1); k += 1
        if (c.io.lgc.rdata.peek().litValue == BigInt(VAL_A)) lat = k
      }
      got = c.io.lgc.rdata.peek().litValue
    }
    (lat, got)
  }

  // 每个配置都要覆盖：单开每一级 + 默认值 + 全开（全开时 5 = cIn+fIn+1+fOut+cOut）
  private val configs = Seq(
    ("全关",            false, false, false, false),
    ("flopIn",          true,  false, false, false),
    ("flopOut",         false, true,  false, false),
    ("CheckIn",         false, false, true,  false),
    ("CheckOut",        false, false, false, true),
    ("默认(out+cOut)",  false, true,  false, true),
    ("全开",            true,  true,  true,  true)
  )

  "Memory 读延时" should "等于 mem.readLatency，且读回请求地址的数据（TP）" in {
    configs.zipWithIndex.foreach { case ((tag, fIn, fOut, cIn, cOut), i) =>
      val m = mk(s"LatTp$i", fIn, fOut, cIn, cOut)
      withClue(s"TpMemoryWrap3[$tag] readLatency=${m.readLatency}：") {
        val (lat, got) = probeTp(m)
        got shouldBe BigInt(VAL_A)
        lat shouldBe m.readLatency
      }
    }
  }

  "Memory 读延时" should "等于 mem.readLatency，且读回请求地址的数据（SP）" in {
    configs.zipWithIndex.foreach { case ((tag, fIn, fOut, cIn, cOut), i) =>
      val m = mk(s"LatSp$i", fIn, fOut, cIn, cOut)
      withClue(s"SpMemoryWrap3[$tag] readLatency=${m.readLatency}：") {
        val (lat, got) = probeSp(m)
        // got 用来兜 CheckIn 只按 we 采样那个 bug：那时读 A 会拿回 B 的值
        got shouldBe BigInt(VAL_A)
        lat shouldBe m.readLatency
      }
    }
  }

  "ECC/Parity" should "不改变读延时（编解码是组合的）" in {
    Seq(MemoryProtectType.ECC, MemoryProtectType.Parity).foreach { p =>
      val m = mk("LatProt", false, false, false, false, p)
      m.readLatency shouldBe 1
      val (tpLat, tpGot) = probeTp(m)
      val (spLat, spGot) = probeSp(m)
      withClue(s"protect=$p：") {
        tpGot shouldBe BigInt(VAL_A); tpLat shouldBe 1
        spGot shouldBe BigInt(VAL_A); spLat shouldBe 1
      }
    }
  }

  "EccErrAddr" should "报出错那次读的地址（而不是下一次读的地址）" in {
    // 延时 2 拍（flopOut=F + CheckOut=T），逐拍抓 eccUerr 脉冲
    val m = mk("ErrAddrTp", false, false, false, true, MemoryProtectType.ECC)
    val seen = scala.collection.mutable.ArrayBuffer[(Int, Int, BigInt)]() // (读地址, 报错拍, 报的地址)
    simulate(new TpMemoryWrap3(m)) { c =>
      c.reset.poke(false.B); c.io.dfx.init.poke(false.B)
      c.io.dfx.injCorrEn.poke(false.B); c.io.dfx.injUerrEn.poke(false.B)
      c.io.lgc.we.poke(false.B); c.io.lgc.re.poke(false.B)
      c.io.lgc.waddr.poke(0.U); c.io.lgc.raddr.poke(0.U); c.io.lgc.wdata.poke(0x1111L.U)
      c.clock.step(4)
      for ((a, d) <- Seq(ADDR_A -> VAL_A, ADDR_B -> VAL_B)) {
        c.io.lgc.we.poke(true.B); c.io.lgc.waddr.poke(a.U); c.io.lgc.wdata.poke(d.U)
        c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)
      }
      def rd(addr: Int, inj: Boolean): Unit = {
        c.io.lgc.re.poke(true.B); c.io.lgc.raddr.poke(addr.U)
        c.io.dfx.injUerrEn.poke(inj.B)
        c.clock.step(1)
        c.io.lgc.re.poke(false.B); c.io.dfx.injUerrEn.poke(false.B)
        var hit = 0
        for (k <- 1 to 8) {
          c.clock.step(1)
          if (hit == 0 && c.io.dfx.eccUerr.peek().litValue == 1) {
            hit = k; seen += ((addr, k, c.io.dfx.eccErrAddr.peek().litValue))
          }
        }
        if (hit == 0) seen += ((addr, 0, -1))
      }
      rd(ADDR_B, false)
      rd(ADDR_A, true)
      rd(ADDR_B, true)
      rd(ADDR_A, true)
    }
    seen.size shouldBe 4
    withClue("未注入 UE 的那次读不应报错：") (seen(0)._2 shouldBe 0)
    seen.drop(1).foreach { case (a, k, ea) =>
      withClue(s"读地址=$a 第 $k 拍报错，eccErrAddr 应为 $a：") {
        k should be > 0
        ea shouldBe BigInt(a)
      }
    }
  }

  "EccErrAddr" should "在 SP 版上同样报出错那次读的地址" in {
    val m = mk("ErrAddrSp", false, false, false, true, MemoryProtectType.ECC)
    val seen = scala.collection.mutable.ArrayBuffer[(Int, Int, BigInt)]()
    simulate(new SpMemoryWrap3(m)) { c =>
      c.reset.poke(false.B); c.io.dfx.init.poke(false.B)
      c.io.dfx.injCorrEn.poke(false.B); c.io.dfx.injUerrEn.poke(false.B)
      c.io.lgc.we.poke(false.B); c.io.lgc.re.poke(false.B)
      c.io.lgc.addr.poke(0.U); c.io.lgc.wdata.poke(0x1111L.U)
      c.clock.step(4)
      for ((a, d) <- Seq(ADDR_A -> VAL_A, ADDR_B -> VAL_B)) {
        c.io.lgc.we.poke(true.B); c.io.lgc.addr.poke(a.U); c.io.lgc.wdata.poke(d.U)
        c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)
      }
      def rd(addr: Int, inj: Boolean): Unit = {
        c.io.lgc.re.poke(true.B); c.io.lgc.addr.poke(addr.U)
        c.io.dfx.injUerrEn.poke(inj.B)
        c.clock.step(1)
        c.io.lgc.re.poke(false.B); c.io.dfx.injUerrEn.poke(false.B)
        var hit = 0
        for (k <- 1 to 8) {
          c.clock.step(1)
          if (hit == 0 && c.io.dfx.eccUerr.peek().litValue == 1) {
            hit = k; seen += ((addr, k, c.io.dfx.eccErrAddr.peek().litValue))
          }
        }
        if (hit == 0) seen += ((addr, 0, -1))
      }
      rd(ADDR_B, true)
      rd(ADDR_A, true)
    }
    seen.foreach { case (a, k, ea) =>
      withClue(s"SP 读地址=$a 第 $k 拍报错，eccErrAddr 应为 $a：") {
        k should be > 0
        ea shouldBe BigInt(a)
      }
    }
  }
}
