package em

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import BaseCbb.memory.MemoryProtectType

// ===========================================================================
// 存储读延时参数化（`EmParams.memFlopIn/Out/CheckIn/Out`）
//
// 读延时 rdLat = CheckIn + flopIn + 1 + flopOut + CheckOut（见 EmLayout.rdLat），
// 流水线各级之间的影子寄存器按它延拍。这里逐个打开每一级、以及默认 Memory 配置（3 拍）
// 和全开（5 拍），验证功能完全不变：
//   · add/lookup/del 往返
//   · 背靠背同 KEY（需求 2，转发 CAM）
//   · 稳态 II=1（流水线填满后每拍都有响应、key_ready 不掉）—— 影子延拍写错最先在这里暴露
//   · 老化（需求 3）—— svc 侧也要等满 rdLat 才能捕获 HT/KT 数据
//
// 用最小配置，避免 Verilator 编译开销堆起来。
// ===========================================================================
class EmRdLatSpec extends AnyFlatSpec with Matchers with EmTestSupport {

  private def paramsFor(tag: String, fIn: Boolean, fOut: Boolean, cIn: Boolean, cOut: Boolean,
                        protect: MemoryProtectType.MemoryProtectType = MemoryProtectType.ProtNone) =
    EmParams(
      keyWidth = 16, adWidth = 26, htDepth = 8, htWays = 2, numBanks = 1,
      dLeftTie = TiePolicy.RoundRobin, ktDepth = 16, adDepth = 16,
      useKt = true, useAd = true,
      learnFwdDepth = 8, slotWaitMax = 8,
      aging = Some(AgingParams(ageWidth = 16, timeout = 4, tickDiv = 2, sweepEnable = true)),
      learning = Some(LearningParams()),
      memProtect = protect,
      memFlopIn = fIn, memFlopOut = fOut, memCheckIn = cIn, memCheckOut = cOut
    )

  private val K1 = BigInt(0x0bad)
  private val A1 = BigInt(0x15)

  // 每个 flag 单独打开一次 + 默认 Memory 配置(3) + 全开(5)
  private val configs = Seq(
    ("flopIn   (rdLat=2)",     paramsFor("flopIn",  true,  false, false, false)),
    ("flopOut  (rdLat=2)",     paramsFor("flopOut", false, true,  false, false)),
    ("CheckIn  (rdLat=2)",     paramsFor("checkIn", false, false, true,  false)),
    ("CheckOut (rdLat=2)",     paramsFor("checkOut",false, false, false, true)),
    ("默认 out+cOut (rdLat=3)", paramsFor("def3",    false, true,  false, true)),
    ("全开 (rdLat=5)",          paramsFor("all5",    true,  true,  true,  true))
  )

  configs.foreach { case (tag, p) =>
    val l = EmLayout(p)

    s"EM[$tag]" should "add/lookup/del + 背靠背转发 + II=1 + 老化 都正常" in {
      withClue(s"rdLat=${l.rdLat}: ") {
        simulate(new ExactMatch(p)) { dut =>
          initDut(dut)

          // ---- add / lookup / del ----
          wrCmd(dut, OP_ADD, K1, A1)
          entries(dut) shouldBe BigInt(1)
          lookup(dut, l, K1) shouldBe ((true, A1))
          wrCmd(dut, OP_ADD, K1, BigInt(0x1aaa))          // 覆盖写
          lookup(dut, l, K1) shouldBe ((true, BigInt(0x1aaa)))

          // ---- 需求 2：背靠背两个同 KEY，第 1 个 miss、第 2 个由转发 CAM 命中 ----
          val K2 = BigInt(0x0c0c)
          val A2 = BigInt(0x22)
          dut.io.learnEn.poke(true.B)
          dut.io.learnAd.poke(A2.U)
          dut.io.key.bits.poke(K2.U)
          dut.io.key.valid.poke(true.B)
          var n = 0
          while (dut.io.key.ready.peek().litValue == 0 && n < 4000) { dut.clock.step(1); n += 1 }
          dut.clock.step(1)                               // A 握手
          dut.io.key.ready.peek().litValue shouldBe 1      // 背靠背：下一拍仍能收
          dut.clock.step(1)                               // B 握手
          dut.io.key.valid.poke(false.B)
          val rsp = scala.collection.mutable.ArrayBuffer[(Boolean, BigInt)]()
          n = 0
          while (rsp.size < 2 && n < 4000) {
            dut.clock.step(1); n += 1
            if (dut.io.rsp.valid.peek().litValue == 1) rsp += splitRsp(l, dut.io.rsp.bits.peek().litValue)
          }
          rsp.size shouldBe 2
          withClue("第 1 个应为 Miss：") (rsp(0)._1 shouldBe false)
          withClue("第 2 个应被转发 CAM 命中：") { rsp(1)._1 shouldBe true; rsp(1)._2 shouldBe A2 }
          dut.io.learnEn.poke(false.B)
          idle(dut)

          // ---- 稳态 II=1：填满流水线后每拍都必须有响应、key_ready 不掉 ----
          val keys = Seq(0x0101, 0x0202, 0x0303, 0x0404).map(BigInt(_))
          keys.zipWithIndex.foreach { case (k, i) => wrCmd(dut, OP_ADD, k, BigInt(0x100 + i)) }
          dut.io.key.valid.poke(true.B)
          var i = 0
          var cyc = 0
          val fill = l.lookupLatency + 4
          while (cyc < fill) { dut.io.key.bits.poke(keys(i % keys.size).U); i += 1; tick(dut); cyc += 1 }
          val win = 24
          var bubbles = 0
          var readyDrop = 0
          cyc = 0
          while (cyc < win) {
            dut.io.key.bits.poke(keys(i % keys.size).U); i += 1
            if (dut.io.rsp.valid.peek().litValue != 1) bubbles += 1
            if (dut.io.key.ready.peek().litValue != 1) readyDrop += 1
            tick(dut); cyc += 1
          }
          dut.io.key.valid.poke(false.B)
          withClue(s"$win 拍内有 $bubbles 拍没有响应（影子延拍错位）：") (bubbles shouldBe 0)
          withClue(s"$win 拍内 key_ready 掉了 $readyDrop 拍：") (readyDrop shouldBe 0)
          var drain = 0
          while (dut.io.rsp.valid.peek().litValue == 1 && drain < 30) { tick(dut); drain += 1 }
          withClue("停灌后未排空：") (drain should be < 30)

          // ---- 需求 3：老化，且 KT/AD 同拍归还（svc 侧也要等满 rdLat）----
          idle(dut)
          dut.io.ageEn.poke(true.B)
          n = 0
          while (entries(dut) != 0 && n < 5000) { tick(dut); n += 1 }
          withClue("老化后条目应清零：") (entries(dut) shouldBe BigInt(0))
          withClue("老化后 KT 应全部归还：") (ktFree(dut) shouldBe BigInt(p.ktDepth))
          withClue("老化后 AD 应全部归还：") (adFreeCnt(dut) shouldBe BigInt(p.adDepth))
          dut.io.ageEn.poke(false.B)
          // 老化后资源确实回收了：重新插入同一个 KEY 必须成功
          wrCmd(dut, OP_ADD, K1, BigInt(0x1111))
          dut.io.status.insFail.peek().litValue shouldBe BigInt(0)
          lookup(dut, l, K1) shouldBe ((true, BigInt(0x1111)))
          wrCmd(dut, OP_DEL, K1, BigInt(0))
          entries(dut) shouldBe BigInt(0)
        }
      }
    }
  }

  // 深流水 + ECC：UE 的 fail-safe 与自愈也必须跟着对齐
  "EM[rdLat=3 + ECC]" should "UE 仍能强制 miss 并自愈" in {
    val p = paramsFor("uerr3", false, true, false, true, MemoryProtectType.ECC)
    val l = EmLayout(p)
    withClue(s"rdLat=${l.rdLat}: ") {
      simulate(new ExactMatch(p)) { dut =>
        initDut(dut)
        wrCmd(dut, OP_ADD, K1, A1)
        entries(dut) shouldBe BigInt(1)
        lookup(dut, l, K1) shouldBe ((true, A1))

        val before = entries(dut)
        val cnt0 = dut.io.status.uerrCnt.peek().litValue
        var pulses = 0

        // 整个查找期间对 KT 注入 UE
        dut.io.key.bits.poke(K1.U)
        dut.io.key.valid.poke(true.B)
        var n = 0
        while (dut.io.key.ready.peek().litValue == 0 && n < 4000) { dut.clock.step(1); n += 1 }
        dut.clock.step(1)
        dut.io.key.valid.poke(false.B)
        dut.io.injUerrEn.poke(true.B)
        dut.io.injUerrSrc.poke(EmUErrSrc.kt)
        var rs: Option[(Boolean, BigInt)] = None
        n = 0
        while (rs.isEmpty && n < 4000) {
          if (dut.io.memUErr.peek().litValue == 1) pulses += 1
          dut.clock.step(1); n += 1
          if (dut.io.rsp.valid.peek().litValue == 1) rs = Some(splitRsp(l, dut.io.rsp.bits.peek().litValue))
        }
        dut.io.injUerrEn.poke(false.B)
        for (_ <- 0 until 12) { if (dut.io.memUErr.peek().litValue == 1) pulses += 1; dut.clock.step(1) }

        withClue("KT 读 UE 必须强制 miss：") (rs.get._1 shouldBe false)
        withClue("必须上报一次 UE：") (pulses shouldBe 1)
        withClue("uerrCnt 应递增：") (dut.io.status.uerrCnt.peek().litValue shouldBe cnt0 + 1)
        withClue("自愈应作废该槽位：") (entries(dut) should be < before)
      }
    }
  }
}
