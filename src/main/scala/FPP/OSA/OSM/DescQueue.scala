package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Per-port packet descriptor queue (docs §3.8).
 *
 * Holds committed PacketDesc of admitted packets per port, FIFO order.
 * Register-based shallow FIFO (depth 16) for this implementation; the full
 * design uses SyncFifo with external SRAM.
 *
 * 修复记录：
 *  - 入队改为位置序链：同一周期内多个报文命中同一端口时，旧实现全部写入
 *    mem(p)(tail(p)) 且 tail 只 +1，导致描述符丢失（后者覆盖前者）。
 *  - deq.valid 不再由 ready 门控（ready→valid→ready 会形成组合环），
 *    改为标准 Decoupled 语义：valid 表示有数据，fire 时出队。
 *  - 入队时分配 pktId（原实现 AdmCtrl 填 0 且无人补写）。
 */
class DescQueue(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val enq   = Flipped(Vec(config.maxNewPktPerCycle, Valid(new PacketDesc)))
    val deq   = Decoupled(new PacketDesc)
    val count = Output(Vec(config.portCount, UInt(5.W)))
  })

  val depth = 16
  val addrW = log2Ceil(depth)              // 4
  val cntW  = log2Ceil(depth) + 1          // 5
  val K     = config.maxNewPktPerCycle

  val head = RegInit(VecInit(Seq.fill(config.portCount)(0.U(addrW.W))))
  val tail = RegInit(VecInit(Seq.fill(config.portCount)(0.U(addrW.W))))
  val cnt  = RegInit(VecInit(Seq.fill(config.portCount)(0.U(cntW.W))))
  val mem  = Reg(Vec(config.portCount, Vec(depth, new PacketDesc)))

  // pktId 分配器（8 位回绕）
  val pktIdCnt = RegInit(0.U(8.W))

  // ---- 入队：位置序链（同周期同端口也能正确累加） --------------------------
  val cntChain  = Seq.fill(K + 1)(Wire(Vec(config.portCount, UInt(cntW.W))))
  val tailChain = Seq.fill(K + 1)(Wire(Vec(config.portCount, UInt(addrW.W))))
  for (p <- 0 until config.portCount) {
    cntChain(0)(p)  := cnt(p)
    tailChain(0)(p) := tail(p)
  }

  val enqOk = Wire(Vec(K, Bool()))
  for (i <- 0 until K) {
    for (p <- 0 until config.portCount) {
      cntChain(i + 1)(p)  := cntChain(i)(p)
      tailChain(i + 1)(p) := tailChain(i)(p)
    }
    val d  = io.enq(i)
    val pt = d.bits.portId
    enqOk(i) := d.valid && cntChain(i)(pt) < depth.U
    when(enqOk(i)) {
      val dsc = Wire(new PacketDesc)
      dsc       := d.bits
      dsc.pktId := pktIdCnt + i.U
      mem(pt)(tailChain(i)(pt)) := dsc
      tailChain(i + 1)(pt) := tailChain(i)(pt) + 1.U
      cntChain(i + 1)(pt)  := cntChain(i)(pt) + 1.U
    }
  }
  for (p <- 0 until config.portCount) {
    tail(p) := tailChain(K)(p)
    cnt(p)  := cntChain(K)(p)
  }
  val enqNum = PopCount(enqOk)
  when(enqNum =/= 0.U) { pktIdCnt := pktIdCnt + enqNum }

  // ---- 出队：端口轮询，标准 Decoupled --------------------------------------
  // 从 selPort 开始做旋转优先级仲裁，跳过空端口：
  // 旧实现只在出队成功时推进 selPort，一旦它停在一个空端口上就再也走不动，
  // 其它端口的描述符会被永久饿死。
  val selPort = RegInit(0.U(log2Ceil(config.portCount).W))
  val rotate = VecInit((0 until config.portCount).map { i =>
    val v = selPort + i.U
    Mux(v >= config.portCount.U, v - config.portCount.U, v)
  })
  val hasAt  = VecInit(rotate.map(p => cnt(p) > 0.U))
  val anyHas = hasAt.reduce(_ || _)
  val nxtPort = rotate(PriorityEncoder(hasAt))

  io.deq.valid := anyHas
  io.deq.bits  := mem(nxtPort)(head(nxtPort))
  when(io.deq.fire) {
    head(nxtPort) := head(nxtPort) + 1.U
    cnt(nxtPort)  := cntChain(K)(nxtPort) - 1.U   // 覆盖上面的入队提交值
    selPort := Mux(nxtPort === (config.portCount - 1).U, 0.U, nxtPort + 1.U)
  }

  for (p <- 0 until config.portCount) io.count(p) := cnt(p)
}
