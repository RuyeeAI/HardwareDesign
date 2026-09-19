package em

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import BaseCbb.SimReset

/**
 * EM 测试公共脚手架（`ExactMatchSpec` 与 `EmWaveSpec` 共用）。
 *
 * 这里只放与"怎么驱动 DUT"有关的东西，不放任何断言 —— 断言留在各自的用例里，
 * 免得两个用例的期望互相污染。
 */
trait EmTestSupport extends org.scalatest.Assertions {

  protected val OP_ADD = 0
  protected val OP_DEL = 1
  protected val OP_UPD = 2

  /** 响应里 hit 是哪一位（`EmLayout` 里定义：ad 的高一位）。 */
  protected def adMask(l: EmLayout): BigInt = (BigInt(1) << l.adW) - 1

  protected def splitRsp(l: EmLayout, v: BigInt): (Boolean, BigInt) =
    (((v >> l.adW) & 1) == 1, v & adMask(l))

  /**
   * 每推进一拍（在 posedge 之前）被调一次，默认什么都不做。
   * 子类可以覆盖它做全局统计 —— 比如 `EmWaveSpec` 用它数"每拍有没有响应"，
   * 这正是以前 `vcd_check.py` 在波形上做的事（波形侧判据不可靠，见 §13.1）。
   * ⚠️ 覆盖后请只做采样/累加，不要在里面 poke（会在推进前改激励）。
   */
  protected def onCycle(dut: ExactMatch): Unit = ()

  /** 推进 n 拍；每一拍都回调 [[onCycle]]。所有驱动都要走这里，统计才完整。 */
  protected def tick(dut: ExactMatch, n: Int = 1): Unit =
    for (_ <- 0 until n) { onCycle(dut); dut.clock.step(1) }

  /** 所有输入拉低/清零，但不复位。 */
  protected def idle(dut: ExactMatch): Unit = {
    dut.io.key.valid.poke(false.B)
    dut.io.key.bits.poke(0.U)
    dut.io.wr.valid.poke(false.B)
    dut.io.wr.bits.op.poke(0.U)
    dut.io.wr.bits.key.poke(0.U)
    dut.io.wr.bits.ad.poke(0.U)
    dut.io.learnAd.poke(0.U)
    dut.io.learnEn.poke(false.B)
    dut.io.ageEn.poke(false.B)
    dut.io.crcPoly.poke(0.U)
    dut.io.crcInit.poke(0.U)
    dut.io.crcXor.poke(0.U)
    dut.io.injUerrEn.poke(false.B)
    dut.io.injUerrSrc.poke(EmUErrSrc.ht)
  }

  /** 复位 + 等存储初始化完成。 */
  protected def initDut(dut: ExactMatch): Unit = {
    SimReset(dut)
    idle(dut)
    dut.io.memInit.poke(true.B)
    tick(dut)
    dut.io.memInit.poke(false.B)
    var n = 0
    while (dut.io.memInitDone.peek().litValue == 0 && n < 60000) { tick(dut); n += 1 }
    dut.io.memInitDone.expect(true.B)
  }

  /** 等 svc 空闲（连续 2 拍 mtBusy=0）。 */
  protected def waitSvcIdle(dut: ExactMatch, maxCyc: Int = 4000): Unit = {
    var n = 0
    var idleCnt = 0
    while (idleCnt < 2 && n < maxCyc) {
      tick(dut); n += 1
      if (dut.io.status.mtBusy.peek().litValue == 0) idleCnt += 1 else idleCnt = 0
    }
  }

  /** 下发一条维护命令并等它做完。 */
  protected def wrCmd(dut: ExactMatch, op: Int, key: BigInt, ad: BigInt, maxCyc: Int = 4000): Unit = {
    dut.io.wr.bits.op.poke(op.U)
    dut.io.wr.bits.key.poke(key.U)
    dut.io.wr.bits.ad.poke(ad.U)
    dut.io.wr.valid.poke(true.B)
    var n = 0
    while (dut.io.wr.ready.peek().litValue == 0 && n < maxCyc) { tick(dut); n += 1 }
    tick(dut)                               // 握手拍
    dut.io.wr.valid.poke(false.B)
    tick(dut)
    waitSvcIdle(dut, maxCyc)
  }

  /**
   * 单次查找，返回 (hit, ad)。
   * ⚠️ 返回的是**第一个** `rsp.valid` 那拍 —— 它只保证"收到了响应"，不保证"只收到一个"。
   * "一个请求恰好一拍响应"由 `EmWaveSpec` 的全局计数断言（`onCycle`）兜底。
   */
  protected def lookup(dut: ExactMatch, l: EmLayout, key: BigInt, maxCyc: Int = 4000): (Boolean, BigInt) = {
    dut.io.key.bits.poke(key.U)
    dut.io.key.valid.poke(true.B)
    var n = 0
    while (dut.io.key.ready.peek().litValue == 0 && n < maxCyc) { tick(dut); n += 1 }
    tick(dut)                               // 握手拍
    dut.io.key.valid.poke(false.B)
    var rs: Option[(Boolean, BigInt)] = None
    n = 0
    while (rs.isEmpty && n < maxCyc) {
      tick(dut); n += 1
      if (dut.io.rsp.valid.peek().litValue == 1) rs = Some(splitRsp(l, dut.io.rsp.bits.peek().litValue))
    }
    rs.getOrElse(fail("lookup 超时，未收到 rsp"))
  }

  protected def entries(dut: ExactMatch): BigInt       = dut.io.status.entries.peek().litValue
  protected def ktFree(dut: ExactMatch): BigInt        = dut.io.status.ktFree.peek().litValue
  protected def adFreeCnt(dut: ExactMatch): BigInt     = dut.io.status.adFree.peek().litValue
  protected def insFail(dut: ExactMatch): BigInt       = dut.io.status.insFail.peek().litValue
}
