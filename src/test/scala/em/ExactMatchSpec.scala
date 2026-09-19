package em

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// ===========================================================================
// EM 流水线版功能测试
//
// 主用例用最小的 "tb" 预设（确定性的 RoundRobin 并列仲裁）：
//   key=16 ad=26, HT = 32 桶 × 2 way × 2 bank = 64 条, OVFC = 8 条, KT = 128(4 bank × 32)
//   老化 timeout=4 / tickDiv=8（即 32 拍超时）
//
// 覆盖三条需求：
//   需求1 每拍一请求             —— 命中流量的 rsp 必须每拍都有（且停灌后必须排空）
//   需求2 背靠背同 KEY 先 Miss 后 Hit —— 转发 CAM
//   需求3 老化并同时释放 KT       —— ktFree/adFree 必须回到满值
// 另有一组按预设参数化的 add/lookup/del 用例，覆盖 useKt/useAd 的四种裁剪组合。
// ===========================================================================
class ExactMatchSpec extends AnyFlatSpec with Matchers with EmTestSupport {

  // 驱动 DUT 的公共脚手架（idle / initDut / wrCmd / lookup / 状态读取）在 EmTestSupport 里，
  // 与 EmWaveSpec 共用一份。

  // =========================================================================
  // 按配置参数化：覆盖 useKt / useAd 的四种裁剪组合
  // 用"小尺寸"参数（而不是 basic/2left 那些大预设）—— 大预设的 Verilator 编译开销太贵，
  // 而这里要验的是**分支裁剪**是否正确，与容量无关。
  // =========================================================================
  private def smallParams(useKt: Boolean, useAd: Boolean, numBanks: Int): EmParams = EmParams(
    keyWidth = 16, adWidth = 26, htDepth = 32, htWays = 2, numBanks = numBanks,
    dLeftTie = TiePolicy.RoundRobin, ktDepth = 128, adDepth = 128,
    useKt = useKt, useAd = useAd
  )

  Seq(
    ("三级 useKt+useAd 1bank", smallParams(true, true, 1)),
    ("单级 inline(useKt=F,useAd=F)", smallParams(false, false, 1)),
    ("两级 noad(useAd=F)", smallParams(true, false, 1)),
    ("三级 useKt+useAd 2bank(d-left)", smallParams(true, true, 2))
  ).foreach { case (cfgName, p) =>
    s"ExactMatch[$cfgName]" should "add/lookup/del/upd 都正确" in {
      val l = EmLayout(p)
      val AD = BigInt(0x1234)
      val K = BigInt(0x0A0B)
      withClue(s"cfg=$cfgName: ") {
        simulate(new ExactMatch(p)) { dut =>
          initDut(dut)
          entries(dut) shouldBe BigInt(0)

          wrCmd(dut, OP_ADD, K, AD)
          entries(dut) shouldBe BigInt(1)
          lookup(dut, l, K) shouldBe ((true, AD))

          // 覆盖同一个 key：动作数据更新，条目数不变
          wrCmd(dut, OP_ADD, K, BigInt(0x777))
          entries(dut) shouldBe BigInt(1)
          lookup(dut, l, K) shouldBe ((true, BigInt(0x777)))

          // 未插入的 key → miss
          lookup(dut, l, BigInt(0x0A0C))._1 shouldBe false

          // upd 未命中 → insFail 递增
          val fail0 = dut.io.status.insFail.peek().litValue
          wrCmd(dut, OP_UPD, BigInt(0x0A0D), BigInt(1))
          dut.io.status.insFail.peek().litValue shouldBe (fail0 + 1)

          // del → 条目与资源都归还
          wrCmd(dut, OP_DEL, K, BigInt(0))
          entries(dut) shouldBe BigInt(0)
          lookup(dut, l, K)._1 shouldBe false
          wrCmd(dut, OP_DEL, K, BigInt(0))       // del 幂等
          entries(dut) shouldBe BigInt(0)
        }
      }
    }
  }

  // =========================================================================
  // 需求2：背靠背两个同 KEY 请求，第 1 个 Miss、第 2 个必须 Hit
  // =========================================================================
  "需求2 背靠背同 KEY" should "第 1 个 Miss 后第 2 个立即 Hit（在途插入转发）" in {
    val p = EmGen.preset("tb")
    val l = EmLayout(p)
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      val K = BigInt(0x0BAD)
      val PORT = BigInt(0x15)
      dut.io.learnEn.poke(true.B)
      dut.io.learnAd.poke(PORT.U)

      // 第 1 拍发 K，第 2 拍（背靠背）再发同一个 K
      dut.io.key.bits.poke(K.U)
      dut.io.key.valid.poke(true.B)
      var n = 0
      while (dut.io.key.ready.peek().litValue == 0 && n < 4000) { dut.clock.step(1); n += 1 }
      dut.clock.step(1)                                  // 请求 A 握手
      dut.io.key.ready.peek().litValue shouldBe 1        // 背靠背：下一拍仍能收
      dut.clock.step(1)                                  // 请求 B 握手
      dut.io.key.valid.poke(false.B)

      val rsp = scala.collection.mutable.ArrayBuffer[(Boolean, BigInt)]()
      n = 0
      while (rsp.size < 2 && n < 4000) {
        dut.clock.step(1); n += 1
        if (dut.io.rsp.valid.peek().litValue == 1) {
          val v = dut.io.rsp.bits.peek().litValue
          rsp += ((((v >> l.adW) & 1) == 1, v & adMask(l)))
        }
      }
      rsp.size shouldBe 2
      withClue("第 1 个请求应为 Miss") { rsp(0)._1 shouldBe false }
      withClue("第 2 个请求应被转发 CAM 命中") {
        rsp(1)._1 shouldBe true
        rsp(1)._2 shouldBe PORT
      }
      // 响应必须"每个请求恰好一拍"：自学习插入会抢时隙冻结流水线，
      // 若不门控 adv，停在 d3 的响应会被保持多拍 → 下游把一个请求数成多个响应。
      // （这条是 review 波形时发现的：rsp_valid 有一次连高 3 拍）
      var extra = 0
      for (_ <- 0 until 20) { dut.clock.step(1); if (dut.io.rsp.valid.peek().litValue == 1) extra += 1 }
      withClue("出现多余响应拍：同一请求被重复上报 ") { extra shouldBe 0 }
      dut.io.learnEn.poke(false.B)
    }
  }

  // =========================================================================
  // 需求1：每拍处理一个请求（命中流量下 II=1）
  // =========================================================================
  "需求1 每拍一请求" should "命中流量下 rsp.valid 连续不间断、且每个响应都是正确命中" in {
    val p = EmGen.preset("tb")
    val l = EmLayout(p)
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      val keys = Seq(0x0101, 0x0202, 0x0303, 0x0404).map(BigInt(_))
      keys.zipWithIndex.foreach { case (k, i) => wrCmd(dut, OP_ADD, k, BigInt(0x100 + i)) }

      // 连续灌 key：valid 一直为 1，不做学习/老化 → 不应该出现任何气泡
      dut.io.key.valid.poke(true.B)
      val fill = l.lookupLatency + 4
      var i = 0
      var cyc = 0
      // 响应按请求顺序回来（流水线 in-order），所以用 expIdx 逐个核对 ad
      var expIdx = 0
      var rspCnt = 0
      var hitBad = 0
      var adBad = 0
      def collect(): Unit = {
        if (dut.io.rsp.valid.peek().litValue == 1) {
          rspCnt += 1
          val v = dut.io.rsp.bits.peek().litValue
          if (((v >> l.adW) & 1) != 1) hitBad += 1
          if ((v & adMask(l)) != BigInt(0x100 + (expIdx % keys.size))) adBad += 1
          expIdx += 1
        }
      }
      while (cyc < fill) { dut.io.key.bits.poke(keys(i % keys.size).U); i += 1; dut.clock.step(1); cyc += 1; collect() }

      val win = 60
      var readyDrop = 0
      val rspBefore = rspCnt
      cyc = 0
      while (cyc < win) {
        dut.io.key.bits.poke(keys(i % keys.size).U); i += 1
        dut.clock.step(1); cyc += 1
        collect()
        if (dut.io.key.ready.peek().litValue == 0) readyDrop += 1
      }
      dut.io.key.valid.poke(false.B)
      withClue(s"$win 拍内只收到 ${rspCnt - rspBefore} 个响应，ready 掉 $readyDrop 拍：") {
        (rspCnt - rspBefore) shouldBe win
        readyDrop shouldBe 0
      }
      withClue(s"有 $hitBad 个响应不是命中、$adBad 个 ad 不符：") {
        hitBad shouldBe 0
        adBad shouldBe 0
      }
      // 反例保护：停灌之后流水线必须排空，rsp.valid 不能一直吊着
      var drain = 0
      while (dut.io.rsp.valid.peek().litValue == 1 && drain < 20) { dut.clock.step(1); drain += 1 }
      withClue("停灌后 rsp.valid 未在有限拍内落下：") { drain should be < 20 }
    }
  }

  // =========================================================================
  // CrcRuntime（运行时可配串行 CRC）模式：覆盖"查找 + 维护"两条串行哈希路径
  //   —— 这条用例正是用来兜 CrcSerial 的 done/out 对齐与 refin 喂位顺序的
  // =========================================================================
  "CrcRuntime 模式" should "串行 CRC 下 add/lookup/del 都正确" in {
    val p = EmParams(
      keyWidth = 16, adWidth = 26, htDepth = 8, htWays = 2, numBanks = 1,
      dLeftTie = TiePolicy.RoundRobin, ktDepth = 16, adDepth = 16,
      useKt = true, useAd = true, crc = CrcRuntime.crc32
    )
    val l = EmLayout(p)
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      // CrcRuntime 模式必须先由 CSR 写好 poly/init/xor —— 否则 poly=0 会让 CRC 恒为 0、
      // 所有 key 挤进同一个桶（既是测试要做的初始化，也是真实的软件约束）
      dut.io.crcPoly.poke(BigInt("04C11DB7", 16).U)
      dut.io.crcInit.poke(BigInt("FFFFFFFF", 16).U)
      dut.io.crcXor.poke(BigInt("FFFFFFFF", 16).U)
      val ks = Seq(0x0A0B, 0x1000, 0x1234).map(BigInt(_))
      ks.zipWithIndex.foreach { case (k, i) => wrCmd(dut, OP_ADD, k, BigInt(0x200 + i)) }
      entries(dut) shouldBe BigInt(3)
      ks.zipWithIndex.foreach { case (k, i) =>
        withClue(f"key=0x$k%04X: ")(lookup(dut, l, k) shouldBe ((true, BigInt(0x200 + i))))
      }
      lookup(dut, l, BigInt(0x0A0C))._1 shouldBe false
      wrCmd(dut, OP_DEL, ks.head, BigInt(0))
      lookup(dut, l, ks.head)._1 shouldBe false
      entries(dut) shouldBe BigInt(2)
    }
  }

  // =========================================================================
  // 指纹冲突：验证"只读 1 路 KT"依然精确（不误报 miss）
  //
  // 设计要点：插入时若候选槽位里已有同指纹条目 → 本次不进 HT
  //   （fpClash++，落 OVFC 或 insFail）。于是任意查找的指纹命中掩码至多 1 位，
  //   "只读命中的那一路 KT" 才成立。
  //
  // 下面这组 key 是**按 DUT 的 CRC 实算挑出来的**（fpW=1 / idxW=3 时）：
  //   (idx=0, fp=1)：0x1003 0x1012 0x1021 0x1030   ← 同桶同指纹，互相撞
  //   (idx=0, fp=0)：0x100b 0x101a 0x1029 0x1038
  // 所以同一个桶里第 2 条起必然触发冲突回避。
  // =========================================================================
  Seq(
    (0, "OVFC 关闭：冲突条目只能插入失败"),
    (2, "OVFC 打开：冲突条目被 OVFC 接住")
  ).foreach { case (ovfcD, label) =>
    s"指纹冲突 fpWidth=1（$label）" should "冲突条目按规则处理，且已插入的 key 不误报 miss / 未插入的不误报 hit" in {
      val p = EmParams(
        keyWidth = 16, adWidth = 26, htDepth = 8, htWays = 2, numBanks = 1,
        dLeftTie = TiePolicy.RoundRobin, ktDepth = 64, adDepth = 64,
        useKt = true, useAd = true, fpWidth = 1, ovfcDepth = ovfcD
      )
      val l = EmLayout(p)
      simulate(new ExactMatch(p)) { dut =>
        initDut(dut)
        val keys = Seq(0x1003, 0x1012, 0x1021, 0x1030, 0x100b, 0x101a, 0x1029, 0x1038)
        var ok = Map.empty[Int, Int]        // key -> ad
        var prevFail = BigInt(0)
        keys.zipWithIndex.foreach { case (k, i) =>
          wrCmd(dut, OP_ADD, BigInt(k), BigInt(0x100 + i))
          val f = dut.io.status.insFail.peek().litValue
          if (f == prevFail) ok += (k -> (0x100 + i))
          prevFail = f
        }
        withClue("必须发生过指纹冲突回避：") (dut.io.status.fpClash.peek().litValue should be > BigInt(0))
        withClue("成功条数应与 entries 一致：") (entries(dut) shouldBe BigInt(ok.size))
        withClue("插入成功的 key 必须全部查得到且 ad 正确：") {
          keys.filter(ok.contains).foreach { k =>
            val (h, a) = lookup(dut, l, BigInt(k))
            withClue(f"key=0x$k%04x ") { h shouldBe true; a shouldBe BigInt(ok(k)) }
          }
        }
        withClue("插入失败的 key 不能误报 hit：") {
          keys.filterNot(ok.contains).foreach { k =>
            withClue(f"key=0x$k%04x ") (lookup(dut, l, BigInt(k))._1 shouldBe false)
          }
        }
      }
    }
  }

  // =========================================================================
  // 需求3：HT 老化，且老化后 KT 资源被同时释放
  // =========================================================================
  "需求3 老化" should "条目过期后被清除，且 KT/AD 资源同拍归还" in {
    val p = EmGen.preset("tb")
    val l = EmLayout(p)
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      val K = BigInt(0x4567)
      wrCmd(dut, OP_ADD, K, BigInt(0x6543))
      entries(dut) shouldBe BigInt(1)
      ktFree(dut) shouldBe BigInt(p.ktDepth - 1)

      // 打开老化，不再访问该条目 → 等它超时被扫掉
      dut.io.ageEn.poke(true.B)
      val drop0 = dut.io.status.ageDrop.peek().litValue
      var n = 0
      while (entries(dut) != 0 && n < 5000) { dut.clock.step(1); n += 1 }

      withClue("老化后条目应清零：") (entries(dut) shouldBe BigInt(0))
      withClue("老化后 KT 资源应全部归还：") (ktFree(dut) shouldBe BigInt(p.ktDepth))
      withClue("老化后 AD 资源应全部归还：") (adFreeCnt(dut) shouldBe BigInt(p.adDepth))
      withClue("ageDrop 应递增：") (dut.io.status.ageDrop.peek().litValue should be > drop0)
      // 关键：空闲数不允许超过总容量（二次释放会把 count 顶爆）
      ktFree(dut) should be <= BigInt(p.ktDepth)
      adFreeCnt(dut) should be <= BigInt(p.adDepth)
      lookup(dut, l, K)._1 shouldBe false

      // 老化后立刻重新插入同一个 KEY 必须成功（证明资源确实回收了）
      wrCmd(dut, OP_ADD, K, BigInt(0x1111))
      dut.io.status.insFail.peek().litValue shouldBe BigInt(0)
      lookup(dut, l, K) shouldBe ((true, BigInt(0x1111)))
      dut.io.ageEn.poke(false.B)
    }
  }

  // =========================================================================
  // 老化与命中并发：反复"边命中边老化"，检查资源计数始终守恒
  // =========================================================================
  "老化与命中并发" should "不产生条目数/空闲数的不一致" in {
    val p = EmGen.preset("tb")
    val l = EmLayout(p)
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      val keys = (0 until 8).map(i => BigInt(0x2000 + i))
      keys.zipWithIndex.foreach { case (k, i) => wrCmd(dut, OP_ADD, k, BigInt(i)) }
      entries(dut) shouldBe BigInt(8)
      ktFree(dut) shouldBe BigInt(p.ktDepth - 8)

      // 打开老化，同时不停查找这些 key（命中会刷新时间戳，理应延缓老化）
      dut.io.ageEn.poke(true.B)
      var cyc = 0
      var i = 0
      var overRelease = false
      while (cyc < 3000) {
        dut.io.key.bits.poke(keys(i % keys.size).U)
        dut.io.key.valid.poke(true.B)
        i += 1
        dut.clock.step(1); cyc += 1
        if (dut.io.key.ready.peek().litValue == 0) { dut.io.key.valid.poke(false.B); dut.clock.step(1); cyc += 1 }
        // 空闲数不得超过总容量（二次释放的典型症状）
        if (ktFree(dut) > BigInt(p.ktDepth) || adFreeCnt(dut) > BigInt(p.adDepth)) overRelease = true
      }
      dut.io.key.valid.poke(false.B)
      overRelease shouldBe false
      dut.io.ageEn.poke(false.B)
    }
  }

  // =========================================================================
  // OVFC：HT 打满后溢出条目仍能查到
  // =========================================================================
  "OVFC 溢出" should "HT 满后新条目落到 OVFC 并能查到" in {
    val p = EmGen.preset("tb")
    val l = EmLayout(p)
    simulate(new ExactMatch(p)) { dut =>
      initDut(dut)
      // HT = 32 桶 × 2 way × 2 bank = 64 条，OVFC = 8 条 → 总容量 72；插 80 条必有溢出与失败
      val n = 80
      var ok = Set.empty[Int]
      var prevFail = BigInt(0)
      for (i <- 0 until n) {
        wrCmd(dut, OP_ADD, BigInt(0x4000 + i), BigInt(i))
        val f = dut.io.status.insFail.peek().litValue
        if (f == prevFail) ok += i
        prevFail = f
      }
      withClue("应该有条目落到 OVFC：") (dut.io.status.ovfcUse.peek().litValue should be > BigInt(0))
      withClue("成功条数应与 entries 一致：") (entries(dut) shouldBe BigInt(ok.size))
      withClue("总条目数不应超过 HT+OVFC 容量：") {
        entries(dut) should be <= BigInt(p.htDepth * p.htWays + p.ovfcDepth)
      }
      withClue("成功插入的 key 必须全部查得到：") {
        ok.toSeq.sorted.foreach { i =>
          val (h, a) = lookup(dut, l, BigInt(0x4000 + i))
          withClue(s"key=0x${(0x4000 + i).toHexString} ") { h shouldBe true; a shouldBe BigInt(i) }
        }
      }
    }
  }
}
