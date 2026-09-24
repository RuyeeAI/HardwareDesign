package em

import chisel3._
import BaseCbb.Sim._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// ===========================================================================
// EM 存储不可纠错误（UE）处理
//
// 注入方式：`io.injUerrEn` + `io.injUerrSrc`（语义同 MemoryDfxPort.injUerrEn），
// 在"发起读"那一拍拉高即让该次读报 UE。查找一次只对目标表发一次读，
// 所以整个查找期间拉高注入 = 恰好注入一次。
//
// 期望行为（fail-safe）：
//   ① 该请求**绝不返回假命中**，ad 一并清零（坏数据不外传）
//   ② memUErr 单拍脉冲 + memUErrSrc 指出来源，uerrCnt 递增
//   ③ UE 自愈：坏掉的 HT 槽位被作废 —— 之后再查同一个 key（且不再注入）应当 miss，
//      条目数相应下降
//
// 用 numBanks=1 + 小表：注入到哪一路没有歧义，自愈结果可判定。
// memProtect 用 ECC：这才是 UE 真实存在的形态（ProtNone 下注入也能报，但没意义）。
// ===========================================================================
class EmUErrSpec extends AnyFlatSpec with Matchers with EmTestSupport {

  private val p = EmParams(
    keyWidth = 16, adWidth = 26, htDepth = 8, htWays = 2, numBanks = 1,
    dLeftTie = TiePolicy.RoundRobin, ktDepth = 64, adDepth = 64,
    useKt = true, useAd = true, memProtect = BaseCbb.memory.MemoryProtectType.ECC
  )
  private val l = EmLayout(p)

  private val K1 = BigInt(0x1234)
  private val K2 = BigInt(0x5678)
  private val A1 = BigInt(0x2abcde)
  private val A2 = BigInt(0x155555)

  private var uePulses = 0
  private var ueSrcSeq = scala.collection.mutable.ArrayBuffer[BigInt]()

  // 每拍（推进前）采样这一拍的 UE 上报 —— 与响应同拍。
  // 必须挂在 onCycle 上而不是自己写循环：wrCmd/waitSvcIdle 走的是 EmTestSupport.tick，
  // 自己写循环会漏掉 svc 路径的脉冲（v1 就是这么漏的）。
  override protected def onCycle(dut: ExactMatch): Unit = {
    if (dut.io.memUErr.peek().litValue == 1) {
      uePulses += 1
      ueSrcSeq += dut.io.memUErrSrc.peek().litValue
    }
  }

  private def resetCounters(): Unit = { uePulses = 0; ueSrcSeq.clear() }

  private def uerrCnt(dut: ExactMatch): BigInt = dut.io.status.uerrCnt.peek().litValue

  /** 单次查找；src 非空则在整个查找期间注入该表的 UE。返回 (hit, ad)。 */
  private def lookupInj(dut: ExactMatch, key: BigInt, src: Option[UInt], maxCyc: Int = 4000): (Boolean, BigInt) = {
    dut.io.key.bits.poke(key.U)
    dut.io.key.valid.poke(true.B)
    var n = 0
    while (dut.io.key.ready.peek().litValue == 0 && n < maxCyc) { cyc(dut); n += 1 }
    cyc(dut)                                   // 握手拍
    dut.io.key.valid.poke(false.B)
    src.foreach { s => dut.io.injUerrEn.poke(true.B); dut.io.injUerrSrc.poke(s) }
    var rs: Option[(Boolean, BigInt)] = None
    n = 0
    while (rs.isEmpty && n < maxCyc) {
      cyc(dut); n += 1
      if (dut.io.rsp.valid.peek().litValue == 1) rs = Some(splitRsp(l, dut.io.rsp.bits.peek().litValue))
    }
    dut.io.injUerrEn.poke(false.B)
    dut.io.injUerrSrc.poke(EmUErrSrc.ht)
    // 收尾几拍，让自愈作废走完
    cyc(dut, 8)
    rs.getOrElse(fail("lookup 超时，未收到 rsp"))
  }

  private def cyc(dut: ExactMatch): Unit = tick(dut)
  private def cyc(dut: ExactMatch, n: Int): Unit = tick(dut, n)

  /** 先用 ECC 配置跑一遍普通读写，确认开了保护也不影响正常命中。 */
  "EM + ECC" should "普通 add/lookup 仍然正确（保护不改变功能）" in {
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      wrCmd(dut, OP_ADD, K1, A1)
      entries(dut) shouldBe BigInt(1)
      resetCounters()
      lookup(dut, l, K1) shouldBe ((true, A1))
      uePulses shouldBe 0
      uerrCnt(dut) shouldBe BigInt(0)
      wrCmd(dut, OP_DEL, K1, BigInt(0))
      lookup(dut, l, K1)._1 shouldBe false
    }
  }

  // KT 的 UE 是最危险的一条：full key 位错会**假命中**返回别人的 ad。
  // 注入只置错误标志、不改数据，所以这条用例验的正是"标志一置就必须不认命中"。
  "KT 读 UE" should "不返回假命中：强制 miss + 上报 + 作废该 HT 槽位" in {
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      wrCmd(dut, OP_ADD, K1, A1)
      entries(dut) shouldBe BigInt(1)
      lookup(dut, l, K1) shouldBe ((true, A1))   // 注入前：正常命中

      val before = entries(dut)
      resetCounters()
      val cnt0 = uerrCnt(dut)
      val (h, ad) = lookupInj(dut, K1, Some(EmUErrSrc.kt))

      withClue("KT 读报 UE 时必须强制 miss（数据看着是好的也不能认命中）：") (h shouldBe false)
      withClue("UE 时 ad 必须清零：") (ad shouldBe BigInt(0))
      withClue("必须上报一次 UE：") (uePulses shouldBe 1)
      withClue("来源应为 KT：") (ueSrcSeq.head shouldBe EmUErrSrc.kt.litValue)
      withClue("uerrCnt 应递增：") (uerrCnt(dut) shouldBe cnt0 + 1)
      withClue("自愈应作废该槽位，条目数下降：") (entries(dut) should be < before)

      // 自愈之后：不再注入也应该 miss（坏槽位已经不在了）
      resetCounters()
      val (h2, _) = lookupInj(dut, K1, None)
      withClue("自愈后再查同一 key 应 miss（且不应再报 UE）：") (h2 shouldBe false)
      withClue("自愈后不应再报 UE：") (uePulses shouldBe 0)
    }
  }

  "HT 读 UE" should "强制 miss + 上报 + 作废该桶" in {
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      wrCmd(dut, OP_ADD, K1, A1)
      lookup(dut, l, K1) shouldBe ((true, A1))

      val before = entries(dut)
      resetCounters()
      val cnt0 = uerrCnt(dut)
      val (h, ad) = lookupInj(dut, K1, Some(EmUErrSrc.ht))

      withClue("HT 读报 UE 时必须强制 miss：") (h shouldBe false)
      withClue("UE 时 ad 必须清零：") (ad shouldBe BigInt(0))
      withClue("必须上报一次 UE：") (uePulses shouldBe 1)
      withClue("来源应为 HT：") (ueSrcSeq.head shouldBe EmUErrSrc.ht.litValue)
      withClue("uerrCnt 应递增：") (uerrCnt(dut) shouldBe cnt0 + 1)
      withClue("自愈应作废整桶（>= 原来那 1 条），条目数下降：") (entries(dut) should be < before)

      resetCounters()
      val (h2, _) = lookupInj(dut, K1, None)
      withClue("自愈后再查同一 key 应 miss：") (h2 shouldBe false)
      withClue("自愈后不应再报 UE：") (uePulses shouldBe 0)
    }
  }

  "AD 读 UE" should "强制 miss 且 ad 清零 + 上报 + 作废该 HT 槽位" in {
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      wrCmd(dut, OP_ADD, K1, A1)
      lookup(dut, l, K1) shouldBe ((true, A1))

      val before = entries(dut)
      resetCounters()
      val cnt0 = uerrCnt(dut)
      val (h, ad) = lookupInj(dut, K1, Some(EmUErrSrc.ad))

      withClue("AD 读报 UE 时必须强制 miss 且 ad=0（绝不能外传坏 ad）：") {
        h shouldBe false
        ad shouldBe BigInt(0)
      }
      withClue("必须上报一次 UE：") (uePulses shouldBe 1)
      withClue("来源应为 AD：") (ueSrcSeq.head shouldBe EmUErrSrc.ad.litValue)
      withClue("uerrCnt 应递增：") (uerrCnt(dut) shouldBe cnt0 + 1)
      withClue("自愈应作废该槽位：") (entries(dut) should be < before)

      resetCounters()
      val (h2, _) = lookupInj(dut, K1, None)
      h2 shouldBe false
      uePulses shouldBe 0
    }
  }

  // 别的 key 不受影响：作废只动坏掉的那一槽/那一桶
  "UE 自愈" should "不误伤别的条目" in {
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      wrCmd(dut, OP_ADD, K1, A1)
      wrCmd(dut, OP_ADD, K2, A2)
      lookup(dut, l, K1) shouldBe ((true, A1))
      lookup(dut, l, K2) shouldBe ((true, A2))

      resetCounters()
      lookupInj(dut, K1, Some(EmUErrSrc.kt)) shouldBe ((false, BigInt(0)))
      uePulses shouldBe 1

      // K2 在别的桶（不同 key 桶索引不同），必须完好
      resetCounters()
      val (h2, a2) = lookupInj(dut, K2, None)
      withClue("自愈不该动到别的 key：") { h2 shouldBe true; a2 shouldBe A2 }
      uePulses shouldBe 0
    }
  }

  // 维护口遇 UE：绝不能用坏数据写回（宁可放弃本次命令）
  "svc 维护读 UE" should "放弃本次任务并上报，不留下半成品条目" in {
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      val e0 = entries(dut)
      resetCounters()
      val cnt0 = uerrCnt(dut)

      // 整个 add 期间对 HT 注入 UE：svc 读 HT 桶时必然报错 → 任务应被放弃
      dut.io.injUerrEn.poke(true.B)
      dut.io.injUerrSrc.poke(EmUErrSrc.ht)
      wrCmd(dut, OP_ADD, K1, A1)
      dut.io.injUerrEn.poke(false.B)

      withClue("条目数不应变化（没插进去，也没留下半成品）：") (entries(dut) shouldBe e0)
      withClue("必须上报 UE：") (uePulses should be > 0)
      withClue("来源应为 HT：") (ueSrcSeq.forall(_ == EmUErrSrc.ht.litValue) shouldBe true)
      withClue("uerrCnt 应递增：") (uerrCnt(dut) should be > cnt0)

      // 关掉注入后重新 add 应当成功（claim 已被 S_MEMERR 放掉，不会卡住）
      resetCounters()
      wrCmd(dut, OP_ADD, K1, A1)
      entries(dut) shouldBe BigInt(1)
      lookup(dut, l, K1) shouldBe ((true, A1))
    }
  }
}
