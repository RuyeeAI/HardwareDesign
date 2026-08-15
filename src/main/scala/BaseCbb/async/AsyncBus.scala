package BaseCbb.async

import chisel3._
import chisel3.util.RegEnable

/**
 * 异步总线跨越（2 相握手 / toggle 协议）。
 *
 * 源域 `srcValid` 时采样 `srcData` 并翻转 req 电平；目标域检测到 req 电平
 * 变化后输出 `dstValid` 脉冲与数据。req/ack 各经两级同步器跨域。
 *
 * 注意：多比特数据直接寄存跨越（简化实现），真实设计需结合
 * 格雷码/数据保持窗口等策略保证数据一致性。
 */
class AsyncBus[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val srcClk   = Input(Clock())
    val srcRst_n = Input(AsyncReset())
    val dstClk   = Input(Clock())
    val dstRst_n = Input(AsyncReset())
    val srcValid = Input(Bool())
    val srcData  = Input(gen.cloneType)
    val dstValid = Output(Bool())
    val dstData  = Output(gen.cloneType)
  })

  val reqSyncOut = Wire(Bool()) // req 同步到目标域后的电平
  val ackSyncIn  = Wire(Bool()) // ack 同步回源域的电平

  // ---- 源域：srcValid 时翻转 req（2 相握手），采样数据 ----
  withClockAndReset(io.srcClk, io.srcRst_n) {
    val reqToggle = RegInit(false.B)
    when(io.srcValid) { reqToggle := ~reqToggle }

    val dataReg = RegEnable(io.srcData, io.srcValid)

    val reqSync = Module(new Sync2(2))
    reqSync.io.clk := io.dstClk
    reqSync.io.rst_n := io.dstRst_n
    reqSync.io.din := reqToggle
    reqSyncOut := reqSync.io.dout

    val ackSync = Module(new Sync2(2))
    ackSync.io.clk := io.srcClk
    ackSync.io.rst_n := io.srcRst_n
    ackSync.io.din := reqSyncOut
    ackSyncIn := ackSync.io.dout

    // 源域可在此等待 ackSyncIn === reqToggle 确认传输完成（本轮不阻塞）
    io.dstData := dataReg // 数据跨域（组合直通，真实设计需同步）
  }

  // ---- 目标域：req 电平变化 → 有效脉冲 ----
  withClockAndReset(io.dstClk, io.dstRst_n) {
    val reqMeta = RegNext(reqSyncOut, false.B)
    io.dstValid := reqSyncOut ^ reqMeta
  }
}
