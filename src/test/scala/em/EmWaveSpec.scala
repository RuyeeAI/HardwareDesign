package em

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// ===========================================================================
// EM 波形场景（Scala 侧驱动，替代原来的手写 Verilog TB）
//
//   sbt "testOnly em.EmWaveSpec"
//
// 跑完在 out/em_wave/workdir-verilator/trace.fst 出波形，用预设视图打开：
//   surfer -c tools/em_tb/em_wave.sucl out/em_wave/workdir-verilator/trace.fst
//
// 场景是原来的 7 个阶段（P1..P7），每个阶段前留一段空闲拍，波形上能直接看出阶段边界；
// 同时把关键观测值 println 出来，没查看器也能读。
//
// 每拍都由 EmTestSupport.tick() 推进，顺带做一次全局采样（见下面 onCycle）：
//   握手拍数（key.valid && key.ready）必须等于响应拍数（rsp.valid 高）
// 这条以前是 vcd_check.py 在波形上查的；svsim 的波形没法做同拍判据，就搬到这儿，
// 而且比波形侧更权威（peek 读的是仿真器的真实值，不是 trace 采样）。
//
// ⚠️ 只在 preset("tb") 上有意义：端口位宽、内部信号名、老化 timeout 都绑这个预设。
// ⚠️ 波形落盘路径（FST）、"必须调 .result" 等，见 WaveSimulator 的注释。
// ===========================================================================
class EmWaveSpec extends AnyFlatSpec with Matchers with EmTestSupport {

  private val p = EmGen.preset("tb")
  private val l = EmLayout(p)

  // 全局响应平衡统计（由 onCycle 累加）
  private var hsCnt  = 0
  private var rspCnt = 0

  override protected def onCycle(dut: ExactMatch): Unit = {
    if (dut.io.key.valid.peek().litValue == 1 && dut.io.key.ready.peek().litValue == 1) hsCnt += 1
    if (dut.io.rsp.valid.peek().litValue == 1) rspCnt += 1
  }

  private def phase(dut: ExactMatch, tag: String, note: => String): Unit = {
    tick(dut, 4)                             // 阶段间留白，波形上分界清晰
    println(f"[$tag] $note")
  }

  "EM 波形场景" should "跑完 P1..P7 并产出波形（需求 1/2/3 + OVFC 都能在波形上看到）" in {
    val sim = new WaveSimulator("out/em_wave")
    val digest = sim.simulateUninitialized(new ExactMatch(p)) { simDut =>
      simDut.controller.setTraceEnabled(true)
      val dut = simDut.wrapped
      hsCnt = 0; rspCnt = 0

      // ---- P1 复位 + 存储初始化 ----
      initDut(dut)
      phase(dut, "P1", s"memInitDone=1 条目=${entries(dut)} KT空闲=${ktFree(dut)}")

      // ---- P2 add（第 2 次覆盖写：指纹命中 → 多走一次 S_KTREQ/S_KTW 读那 1 路 KT）----
      wrCmd(dut, OP_ADD, BigInt(0x1234), BigInt(0x2abcde))
      phase(dut, "P2", s"add(0x1234) 条目=${entries(dut)} KT空闲=${ktFree(dut)}")
      wrCmd(dut, OP_ADD, BigInt(0x1234), BigInt(0x0777))
      phase(dut, "P2", s"add(0x1234) 覆盖 条目=${entries(dut)} 指纹撞=${dut.io.status.fpClash.peek().litValue}")

      // ---- P3 命中查找 / 未插入查找 ----
      val (h3a, ad3a) = lookup(dut, l, BigInt(0x1234))
      phase(dut, "P3", f"lookup(0x1234) -> hit=$h3a ad=0x$ad3a%x")
      val (h3b, ad3b) = lookup(dut, l, BigInt(0x9999))
      phase(dut, "P3", f"lookup(0x9999) -> hit=$h3b ad=0x$ad3b%x")
      h3a shouldBe true
      ad3a shouldBe BigInt(0x0777)
      h3b shouldBe false

      // ---- P4 背靠背两个同 KEY（需求 2）----
      dut.io.learnEn.poke(true.B)
      dut.io.learnAd.poke(BigInt(0x15).U)
      dut.io.key.bits.poke(BigInt(0x0bad).U)
      dut.io.key.valid.poke(true.B)
      var n = 0
      while (dut.io.key.ready.peek().litValue == 0 && n < 4000) { tick(dut); n += 1 }
      tick(dut)                                // 请求 A 握手
      val readyB2b = dut.io.key.ready.peek().litValue
      tick(dut)                                // 请求 B 背靠背握手
      dut.io.key.valid.poke(false.B)
      withClue("背靠背：A 握手后下一拍 key_ready 必须仍为 1：") (readyB2b shouldBe 1)

      val rsp4 = scala.collection.mutable.ArrayBuffer[(Boolean, BigInt)]()
      n = 0
      while (rsp4.size < 2 && n < 4000) {
        tick(dut); n += 1
        if (dut.io.rsp.valid.peek().litValue == 1) rsp4 += splitRsp(l, dut.io.rsp.bits.peek().litValue)
      }
      rsp4.size shouldBe 2
      phase(dut, "P4", s"第1个 hit=${rsp4(0)._1} / 第2个 hit=${rsp4(1)._1} ad=0x${rsp4(1)._2.toString(16)}" +
        s" 转发CAM占用=${dut.io.status.fwdUse.peek().litValue}")
      withClue("第 1 个请求应为 Miss：") (rsp4(0)._1 shouldBe false)
      withClue("第 2 个请求应被转发 CAM 命中：") {
        rsp4(1)._1 shouldBe true
        rsp4(1)._2 shouldBe BigInt(0x15)
      }
      // 响应必须"每个请求恰好一拍"（svc 抢拍冻流水线时 rsp_valid 曾被保持多拍）。
      // 这里顺手看接下来 20 拍有没有多余的响应拍，全局计数再兜一道。
      val extra0 = rspCnt
      tick(dut, 20)
      withClue(s"出现 ${rspCnt - extra0} 拍多余响应（同一请求被重复上报）：") (rspCnt shouldBe extra0)
      dut.io.learnEn.poke(false.B)
      idle(dut)

      // ---- P5 连续灌命中流量（需求 1：II=1）----
      val keys = Seq(0x0101, 0x0202, 0x0303, 0x0404).map(BigInt(_))
      keys.zipWithIndex.foreach { case (k, i) => wrCmd(dut, OP_ADD, k, BigInt(0x100 + i)) }

      dut.io.key.valid.poke(true.B)
      var i = 0
      var cyc = 0
      // 先灌满流水线（查找延迟 + 余量），这段时间不判 II
      val fill = l.lookupLatency + 4
      while (cyc < fill) { dut.io.key.bits.poke(keys(i % keys.size).U); i += 1; tick(dut); cyc += 1 }

      // 稳态窗口：**每一拍**都必须有响应落地、且 key_ready 不得掉。
      // 采样在 tick 之前（与本用例 onCycle 的口径一致）：读到的是"本拍呈现的值"，
      // 也就是这个 posedge 会上报的那个响应 —— 一个响应恰好占一拍。
      val win = 40
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
      phase(dut, "P5", s"$win 拍稳态窗口：气泡 $bubbles 拍，key_ready 掉 $readyDrop 拍")
      withClue(s"$win 拍内有 $bubbles 拍没有响应（II≠1）：") (bubbles shouldBe 0)
      withClue(s"$win 拍内 key_ready 掉了 $readyDrop 拍：") (readyDrop shouldBe 0)
      // 停灌后流水线必须排空
      var drain = 0
      while (dut.io.rsp.valid.peek().litValue == 1 && drain < 20) { tick(dut); drain += 1 }
      withClue("停灌后 rsp.valid 未在有限拍内落下：") (drain should be < 20)

      // ---- P6 老化（需求 3）----
      phase(dut, "P6", s"打开老化前 条目=${entries(dut)} KT空闲=${ktFree(dut)} AD空闲=${adFreeCnt(dut)}")
      val e0 = entries(dut)
      val drop0 = dut.io.status.ageDrop.peek().litValue
      dut.io.ageEn.poke(true.B)
      n = 0
      while (entries(dut) != 0 && n < 5000) { tick(dut); n += 1 }
      phase(dut, "P6", s"老化完成（等 $n 拍）条目=${entries(dut)} KT空闲=${ktFree(dut)}" +
        s" AD空闲=${adFreeCnt(dut)} 老化删除=${dut.io.status.ageDrop.peek().litValue - drop0}")
      dut.io.ageEn.poke(false.B)
      withClue("老化后条目应清零：") (entries(dut) shouldBe BigInt(0))
      withClue("老化后 KT 资源应全部归还：") (ktFree(dut) shouldBe BigInt(p.ktDepth))
      withClue("老化后 AD 资源应全部归还：") (adFreeCnt(dut) shouldBe BigInt(p.adDepth))
      withClue("老化前后应有条目被清掉：") (e0 should be > BigInt(0))
      lookup(dut, l, BigInt(0x1234))._1 shouldBe false

      // ---- P7 打满 HT + OVFC ----
      val fail0 = insFail(dut)
      for (i <- 0 until 80) wrCmd(dut, OP_ADD, BigInt(0x4000 + i), BigInt(i))
      phase(dut, "P7", s"插 80 条：条目=${entries(dut)} OVFC占用=${dut.io.status.ovfcUse.peek().litValue}" +
        s" 指纹撞=${dut.io.status.fpClash.peek().litValue} 本次插失败=${insFail(dut) - fail0}")
      withClue("应该有条目落到 OVFC：") (dut.io.status.ovfcUse.peek().litValue should be > BigInt(0))
      withClue("总条目数不应超过 HT+OVFC 容量：") {
        entries(dut) should be <= BigInt(p.htDepth * p.htWays + p.ovfcDepth)
      }
      lookup(dut, l, BigInt(0x4000)) shouldBe ((true, BigInt(0)))
      val (h7, ad7) = lookup(dut, l, BigInt(0x4001))
      phase(dut, "P7", f"lookup(0x4000)->hit / lookup(0x4001)->hit=$h7 ad=$ad7")

      // 收尾留白：让波形尾部有一段稳定的空闲，便于在查看器里定位最后状态
      tick(dut, 10)
      println(s"[done] 条目=${entries(dut)} KT空闲=${ktFree(dut)} AD空闲=${adFreeCnt(dut)}")
      println(s"[balance] 握手拍=$hsCnt 响应拍=$rspCnt")
    }
    // ⚠️ 必须调 result：否则编译/仿真异常被吞在 digest 里，表现为"测试通过但没波形"
    digest.result

    // 整条波形上"一个请求恰好一拍响应"（原先 vcd_check.py 的检查，搬到这里）
    withClue(s"响应不平衡：$hsCnt 个握手 vs $rspCnt 拍响应（差 ${rspCnt - hsCnt}）" +
      " —— 检查 rsp.valid 是否被 adv 门控：") (hsCnt shouldBe rspCnt)

    val vcd = new java.io.File(sim.tracePath)
    withClue(s"波形未生成：${sim.tracePath}（检查 verilator 是否可用）") (vcd.exists() shouldBe true)
    withClue("波形文件为空：") (vcd.length() should be > 1000L)
    println(s"[wave] ${sim.tracePath}  (${vcd.length() / 1024} KB)")
    println(s"[wave] surfer -c tools/em_tb/em_wave.sucl ${sim.tracePath}")
  }
}
