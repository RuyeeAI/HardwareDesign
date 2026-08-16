package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Dedicated loopback TP memory (docs §3.15).
 *
 * 8 banks x 32B dual-port per loopback port, separate from the main buffer.
 * The TP ports provide 256 B/cycle write (injection) and 256 B/cycle read
 * (egress) simultaneously. This implementation models the storage with
 * SyncReadMem plus a one-word output stage so the egress handshake is lossless:
 * a word is fetched from the memory only when the stage is empty, and the
 * stage is cleared only when the downstream (egress scheduler) accepts it.
 */
class LoopbackMemory(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val wrEn    = Input(Bool())                      // injection valid (one 32B word)
    val wrData  = Input(UInt(config.loopBankWidthB.W))
    val wrAddr  = Input(UInt(log2Ceil(config.loopMemDepth).W))
    val rdReady = Input(Bool())                      // downstream (egress) accepts
    val rdEn    = Output(Bool())                     // egress valid (word available)
    val rdData  = Output(UInt(config.loopBankWidthB.W))
    val level   = Output(UInt(9.W))
  })

  val B = config.loopBankCount   // 8
  val depth = config.loopMemDepth

  // 8 banks x 32B; word index -> bank = idx mod 8, row = idx / 8
  val mems = Seq.fill(B)(SyncReadMem(depth / B, UInt(config.loopBankWidthB.W)))

  val wrPtr = RegInit(0.U(10.W))
  val rdPtr = RegInit(0.U(10.W))
  val cnt   = RegInit(0.U(9.W))

  val wrRow = (wrPtr / B.U(10.W))(log2Ceil(depth / B) - 1, 0)
  val rdRow = (rdPtr / B.U(10.W))(log2Ceil(depth / B) - 1, 0)

  val wrBank = wrPtr % B.U(10.W)
  val rdBank = rdPtr % B.U(10.W)

  when(io.wrEn) {
    for (i <- 0 until B) {
      when(wrBank === i.U) { mems(i).write(wrRow, io.wrData) }
    }
    wrPtr := wrPtr + 1.U
  }

  // ---- egress: fetch -> stage -> present, lossless handshake ---------------
  // stage holds the word currently presented on rdData; it is filled one cycle
  // after a fetch (SyncReadMem read latency) and drained on downstream accept.
  val stageValid = RegInit(false.B)
  val stageData  = RegInit(0.U(config.loopBankWidthB.W))
  val consumed   = stageValid && io.rdReady
  val fetchReq   = !stageValid && cnt > 0.U
  // The read output reflects the bank selected one cycle earlier (address is
  // latched at the fetch edge, rdPtr advances in the same cycle), so select
  // the response with RegNext(rdBank) — same alignment as BufRdCtrl.prevBank.
  val prevBank   = RegNext(rdBank)
  val memData    = Mux1H((0 until B).map(i => prevBank === i.U),
                         mems.map(m => m.read(rdRow, fetchReq)))
  val fetchReq_d = RegNext(fetchReq)

  cnt := cnt + Mux(io.wrEn, 1.U, 0.U) - Mux(fetchReq, 1.U, 0.U)
  when(fetchReq)   { rdPtr := rdPtr + 1.U }
  when(fetchReq_d) { stageValid := true.B; stageData := memData }
  when(consumed)   { stageValid := false.B }

  io.rdEn   := stageValid
  io.rdData := stageData
  io.level  := cnt
}
