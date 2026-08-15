package BaseCbb.async

import chisel3._

/**
 * 异步脉冲跨越（request-acknowledge 4 相握手）。
 *
 * 源时钟域脉冲 `pulseIn` 经 req/ack 握手跨时钟域，在目标时钟域输出
 * 单周期脉冲 `pulseOut`。req 与 ack 各经两级同步器（Sync2）跨域。
 */
class AsyncPulse extends Module {
  val io = IO(new Bundle {
    val srcClk   = Input(Clock())
    val srcRst_n = Input(AsyncReset())
    val dstClk   = Input(Clock())
    val dstRst_n = Input(AsyncReset())
    val pulseIn  = Input(Bool())
    val pulseOut = Output(Bool())
  })

  val reqSyncOut = Wire(Bool()) // req 同步到目标域后的电平
  val ackSyncIn  = Wire(Bool()) // ack 同步回源域的电平

  // ---- 源域：脉冲置位 req，ack 返回后清除（4 相握手）----
  withClockAndReset(io.srcClk, io.srcRst_n) {
    val reqReg = RegInit(false.B)
    when(io.pulseIn) { reqReg := true.B }
    when(ackSyncIn) { reqReg := false.B }

    // req 同步到目标域
    val reqSync = Module(new Sync2(2))
    reqSync.io.clk := io.dstClk
    reqSync.io.rst_n := io.dstRst_n
    reqSync.io.din := reqReg
    reqSyncOut := reqSync.io.dout

    // ack 同步回源域
    val ackSync = Module(new Sync2(2))
    ackSync.io.clk := io.srcClk
    ackSync.io.rst_n := io.srcRst_n
    ackSync.io.din := reqSyncOut
    ackSyncIn := ackSync.io.dout
  }

  // ---- 目标域：req 上升沿 → 单周期脉冲 ----
  withClockAndReset(io.dstClk, io.dstRst_n) {
    val reqMeta = RegNext(reqSyncOut, false.B)
    io.pulseOut := reqSyncOut && !reqMeta
  }
}
