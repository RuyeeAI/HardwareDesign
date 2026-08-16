package FPP.OSA.OSM

import chisel3._
import chisel3.util._
import BaseCbb.data.GenModule

/**
 * Segment demultiplexer (docs §3.1).
 *
 * Scans the 20-segment input stream for up to 3 SOPs per cycle, allocates a
 * context slot to each new packet (position-ordered, via PktCtxAlloc), and
 * tags every segment with {portId, slotId, sop, eop, err, drop}. Also emits
 * up to 3 new-packet windows (first 32B) for the PPRS bank.
 */
class SegDemux(config: OSAConfig) extends GenModule {
  val io = IO(new Bundle {
    val mac     = Flipped(new InterfaceMacOsa(config))

    // tagged segment stream -> write path / assembler
    val segs    = Output(Vec(config.segmentsPerCycle, new TaggedSeg))

    // new-packet dispatch -> PPRS bank (<= 3 per cycle)
    val newPkt  = Vec(config.maxNewPktPerCycle, new NewPacketWindow)
    val newPktValid = Vec(config.maxNewPktPerCycle, Bool())

    // context allocation (PktCtxAlloc)
    val allocReq   = Output(Vec(config.maxNewPktPerCycle, Valid(UInt(log2Ceil(config.portCount).W))))
    val allocGrant = Input(Vec(config.maxNewPktPerCycle, Valid(UInt(log2Ceil(config.ctxPerPort).W))))

    val sopOverflow = Output(Bool())
  })

  val N = config.segmentsPerCycle
  val slotW = log2Ceil(config.ctxPerPort)
  val portW = log2Ceil(config.portCount)

  // ---- SOP detection (<= 3) ----------------------------------------------
  val sopMask = Wire(Vec(N, Bool()))
  for (p <- 0 until N) sopMask(p) := io.mac.sop(p) && io.mac.valid(p)

  // sopRank(p) = number of SOPs before position p
  val sopRank = Wire(Vec(N, UInt(log2Ceil(config.maxNewPktPerCycle + 1).W)))
  for (p <- 0 until N) {
    sopRank(p) := PopCount(sopMask.slice(0, p))  // O(N^2), fine for N=20
  }

  // position of the i-th SOP
  val sopAt = Wire(Vec(config.maxNewPktPerCycle, Vec(N, Bool())))
  val sopPos = Wire(Vec(config.maxNewPktPerCycle, UInt(log2Ceil(N).W)))
  val sopPort = Wire(Vec(config.maxNewPktPerCycle, UInt(portW.W)))
  for (i <- 0 until config.maxNewPktPerCycle) {
    for (p <- 0 until N) sopAt(i)(p) := sopMask(p) && sopRank(p) === i.U
    sopPos(i) := OHToUInt(sopAt(i))
    sopPort(i) := Mux1H(sopAt(i), io.mac.portId)
  }

  // requests to PktCtxAlloc, in stream order
  for (i <- 0 until config.maxNewPktPerCycle) {
    io.allocReq(i).valid := sopAt(i).reduce(_ || _)
    io.allocReq(i).bits  := sopPort(i)
  }

  // grant per SOP position: which request index does position p's SOP map to?
  // slotOfSop(p) = allocGrant(rank(p)).bits if grant valid
  val slotOfSop = Wire(Vec(N, UInt(slotW.W)))
  val grantOfSop = Wire(Vec(N, Bool()))
  for (p <- 0 until N) {
    slotOfSop(p) := MuxLookup(sopRank(p), 0.U)(
      (0 until config.maxNewPktPerCycle).map(i => i.U -> io.allocGrant(i).bits))
    grantOfSop(p) := MuxLookup(sopRank(p), false.B)(
      (0 until config.maxNewPktPerCycle).map(i => i.U -> io.allocGrant(i).valid))
  }

  // 4th+ SOP in a cycle (rank >= maxNewPktPerCycle) is an overflow drop
  val overflowMask = Wire(Vec(N, Bool()))
  for (p <- 0 until N) overflowMask(p) := sopMask(p) && sopRank(p) >= config.maxNewPktPerCycle.U
  io.sopOverflow := overflowMask.reduce(_ || _)

  // ---- per-port current-slot tracking (position-ordered) -----------------
  val curSlot = RegInit(VecInit(Seq.fill(config.portCount)(0.U(slotW.W))))
  val curDrop = RegInit(VecInit(Seq.fill(config.portCount)(false.B)))

  val runSlot = Wire(Vec(config.portCount, UInt(slotW.W)))
  val runDrop = Wire(Vec(config.portCount, Bool()))
  for (p <- 0 until config.portCount) { runSlot(p) := curSlot(p); runDrop(p) := curDrop(p) }

  // ---- tagged segment stream ---------------------------------------------
  for (p <- 0 until N) {
    val port = io.mac.portId(p)
    // position-ordered update: if this position starts a packet, the port's
    // running slot switches to the newly allocated slot
    when(sopMask(p)) {
      runSlot(port) := slotOfSop(p)
      runDrop(port) := !grantOfSop(p) || overflowMask(p)
    }
    // a non-SOP segment belongs to the port's currently open packet
    io.segs(p).data   := io.mac.data(p)   // 8B segment
    io.segs(p).byteEn := 0xFF.U
    io.segs(p).portId := port
    io.segs(p).slotId := runSlot(port)
    io.segs(p).sop    := io.mac.sop(p)
    io.segs(p).eop    := io.mac.eop(p)
    io.segs(p).err    := io.mac.err(p)
    io.segs(p).drop   := runDrop(port)
    io.segs(p).valid  := io.mac.valid(p)
  }

  // register the running slot/drop state at end of cycle
  for (p <- 0 until config.portCount) {
    curSlot(p) := runSlot(p)
    curDrop(p) := runDrop(p)
  }

  // ---- new-packet windows (first 32B) ------------------------------------
  for (i <- 0 until config.maxNewPktPerCycle) {
    val pos = sopPos(i)
    // first32B = 4 segments starting at the SOP position (bytes 0..31)
    val bytes = Wire(Vec(32, UInt(8.W)))
    for (b <- 0 until 32) {
      val segIdx = pos + (b / 8).asUInt
      val inRange = segIdx < N.U
      bytes(b) := Mux(inRange && io.mac.valid(segIdx), io.mac.data(segIdx), 0.U(8.W))
    }
    io.newPkt(i).portId   := sopPort(i)
    io.newPkt(i).slotId   := io.allocGrant(i).bits
    io.newPkt(i).first32B := Cat(bytes.reverse)
    io.newPkt(i).sopPos   := pos
    io.newPktValid(i)     := io.allocReq(i).valid
  }
}
