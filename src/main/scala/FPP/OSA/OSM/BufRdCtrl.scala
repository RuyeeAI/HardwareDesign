package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Buffer read control (docs §3.10).
 *
 * Issues up to 24 segment reads per cycle from the next read address. In the
 * full design the scheduler is conflict-aware (skips banks in `wrMask`) with
 * a pending-read queue and a ReorderQueue; this first implementation issues
 * reads directly (consecutive addresses map to distinct banks for a single
 * packet stream) and returns the responses in issue order. `rdEn` gates the
 * issue (backpressure from the egress).
 */
class BufRdCtrl(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val rdBase = Input(UInt(config.bufAddrWidth.W))  // next read address (packet payload)
    val rdEn   = Input(Bool())                       // egress wants a beat
    // 本拍有效段数（<= N）：报文尾拍按实际剩余段数截断，避免越界读到下一个报文
    val segLimit = Input(UInt(log2Ceil(config.outSegPerBeat + 1).W))
    val lastBeat = Input(Bool())                    // 本拍是该报文的最后一拍（尾拍产生 isEOP）
    val wrMask = Input(UInt(config.banks.W))         // banks written this cycle (write-priority)
    val rdReq  = Output(Vec(config.banks, Valid(new BankRdReq(config))))
    val rdResp = Flipped(Vec(config.banks, Valid(new BankRdResp(config))))
    val rdData = Output(Valid(new BufReadDataVec(config)))
  })

  val N = config.outSegPerBeat   // 24
  val B = config.banks

  // issue up to N reads from rdBase
  val reqAddr = Wire(Vec(N, UInt(config.bufAddrWidth.W)))
  val reqBank = Wire(Vec(N, UInt(log2Ceil(B).W)))
  val reqRow  = Wire(Vec(N, UInt(config.bankRowAddrW.W)))
  val reqValid = Wire(Vec(N, Bool()))
  for (i <- 0 until N) {
    reqAddr(i) := io.rdBase + i.U
    reqBank(i) := reqAddr(i) % B.U(config.bufAddrWidth.W)
    reqRow(i)  := (reqAddr(i) / B.U(config.bufAddrWidth.W))(config.bankRowAddrW - 1, 0)
    reqValid(i) := io.rdEn && (i.U < io.segLimit)
  }

  // per-bank request mux: at most one segment per bank (consecutive addresses
  // map to distinct banks while N < B, so this is conflict-free by construction)
  for (b <- 0 until B) {
    val hits = (0 until N).map(i => reqBank(i) === b.U && reqValid(i))
    io.rdReq(b).valid := hits.reduce(_ || _)
    io.rdReq(b).bits.addr := Mux1H(hits, reqRow)
    io.rdReq(b).bits.tag  := OHToUInt(hits)   // position index as tag
  }

  // responses return after 1 cycle, aligned per bank; reassemble by position
  val prevBank = RegNext(reqBank)
  val prevValid = RegNext(reqValid)
  val prevLimit = RegNext(io.segLimit)
  val prevLast  = RegNext(io.lastBeat, false.B)
  for (i <- 0 until N) {
    io.rdData.bits.segs(i).data   := io.rdResp(prevBank(i)).bits.data
    io.rdData.bits.segs(i).byteEn := 0xFF.U
    io.rdData.bits.segs(i).isSOP  := (i.U === 0.U) && prevValid(i)  // first segment of the beat
    // 尾拍的最后一个有效段才是报文结束（segLimit == N 的全满拍不一定是尾拍）
    io.rdData.bits.segs(i).isEOP  := prevLast && prevValid(i) &&
                                     (i.U === prevLimit - 1.U)
    io.rdData.bits.segs(i).err    := io.rdResp(prevBank(i)).bits.uecErr
    io.rdData.bits.segs(i).valid  := io.rdResp(prevBank(i)).valid && prevValid(i) &&
                                     (i.U < prevLimit)
    io.rdData.bits.segs(i).portId := 0.U
    io.rdData.bits.segs(i).pktId  := 0.U
  }
  io.rdData.valid := prevValid.reduce(_ || _)
}
