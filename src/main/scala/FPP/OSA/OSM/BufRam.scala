package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Shared buffer SRAM — 44 banks x 8B (docs §3.7 / §6.1).
 *
 * Each bank is a 2560-row x 64-bit memory. This implementation uses
 * SyncReadMem as a dual-port behavioral model (1 write + 1 read per bank per
 * cycle) — the SP write-priority arbitration of the real design is modeled at
 * the control level (BufRdCtrl skips banks in `wrMask`); the physical SRAM
 * wrapper (SpMemoryWrap3) is a drop-in replacement at integration time.
 */
class BufRam(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val wrReq  = Vec(config.banks, Flipped(new BankWrReq(config)))
    val rdReq  = Vec(config.banks, Flipped(Valid(new BankRdReq(config))))
    val rdResp = Output(Vec(config.banks, Valid(new BankRdResp(config))))
  })

  val mems = Seq.fill(config.banks)(SyncReadMem(config.rowsPerBank, UInt(64.W)))

  for (b <- 0 until config.banks) {
    when(io.wrReq(b).we) {
      mems(b).write(io.wrReq(b).addr, io.wrReq(b).data)
    }
    io.rdResp(b).valid := io.rdReq(b).valid
    io.rdResp(b).bits.data   := mems(b).read(io.rdReq(b).bits.addr, io.rdReq(b).valid)
    io.rdResp(b).bits.tag    := io.rdReq(b).bits.tag
    io.rdResp(b).bits.uecErr := false.B
  }
}
