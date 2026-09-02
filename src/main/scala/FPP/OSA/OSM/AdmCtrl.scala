package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Admission control (docs §3.9).
 *
 * Decides per completed packet (EOP & priority valid) whether to forward,
 * drop (lossy over threshold), or backpressure (lossless over threshold).
 * Forwarding writes the PacketDesc into the port's DescQueue; a drop
 * requests a buffer rollback (wrPtr rewind by segCount).
 */
class AdmCtrl(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val done       = Flipped(Vec(config.maxNewPktPerCycle, Valid(new PktAssemblyDone)))
    val occupancy  = Input(Vec(config.portCount, UInt(config.bufAddrWidth.W)))
    val thresholds = Input(Vec(config.portCount, new PortThresholds))
    // 每个 context 当前报文的首段缓冲地址（来自 BufWrPath），用于填 PacketDesc.bufBase
    val ctxStart   = Input(Vec(config.ctxPool, UInt(config.bufAddrWidth.W)))
    val fwd        = Output(Vec(config.maxNewPktPerCycle, Valid(new PacketDesc)))
    val rollback   = Output(Vec(config.maxNewPktPerCycle, Valid(new RollbackInfo)))
    val bpEvent    = Output(Bool())
  })

  for (i <- 0 until config.maxNewPktPerCycle) {
    val d = io.done(i)
    val port = d.bits.portId
    val thr = io.thresholds(port)
    val occ = io.occupancy(port)

    // priority class: 0=lossy low, 1=lossy high, 2=lossless low, 3=lossless high
    val isLossy = d.bits.priClass(1) === 0.U
    val isLossyLow = d.bits.priClass(1, 0) === 0.U
    val dropLossyLow  = isLossy && isLossyLow && occ > thr.lossyLow
    val dropLossyHigh = isLossy && !isLossyLow && occ > thr.lossyHigh
    val bpLossless    = !isLossy && occ > thr.lossless

    val tooSmall = d.bits.tooSmall
    val drop = (dropLossyLow || dropLossyHigh || tooSmall || bpLossless) && d.valid

    // forward descriptor
    io.fwd(i).valid := d.valid && !drop
    io.fwd(i).bits.portId    := d.bits.portId
    io.fwd(i).bits.pktId     := 0.U          // assigned by DescQueue
    io.fwd(i).bits.macHeader := d.bits.macHeader
    io.fwd(i).bits.byteCount := d.bits.byteCount
    io.fwd(i).bits.segCount  := d.bits.segCount
    // 报文首地址：context = portId * ctxPerPort + slotId
    io.fwd(i).bits.bufBase   := io.ctxStart(d.bits.portId * config.ctxPerPort.U + d.bits.slotId)
    io.fwd(i).bits.orgQindex := d.bits.orgQindex
    io.fwd(i).bits.priClass  := d.bits.priClass
    io.fwd(i).bits.err       := d.bits.err

    // rollback on drop
    io.rollback(i).valid := drop
    io.rollback(i).bits.portId := d.bits.portId
    io.rollback(i).bits.segCount := d.bits.segCount
  }

  io.bpEvent := io.done.map(d => d.valid && d.bits.priClass(1) === 1.U &&
                              io.occupancy(d.bits.portId) > io.thresholds(d.bits.portId).lossless)
    .reduce(_ || _)
}
