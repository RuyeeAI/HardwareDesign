package FPP.OSA.OSM

/**
 * OSA configuration — mirrors docs/OSA.md §5.1 (v2.2).
 *
 * Key parameters:
 *  - 44-bank single-port main buffer (20 write + 24 read peak, 2 × 96B egress)
 *  - 2 loopback ports, each rate-limited to 300 Gbps, stored in dedicated
 *    8 × 32B TP memories (separate from the main buffer)
 *  - work-conserving egress: OSA read is strict priority, loopbacks use the
 *    leftover egress with token-bucket rate limiting
 */
case class OSAConfig(
  portCount: Int = 8,             // network ports (lane granularity 200G steps)
  segmentsPerCycle: Int = 20,     // input segments per cycle (1.6 Tbps @1.25GHz)
  banks: Int = 44,                // main buffer banks (SP), bank = addr mod 44
  bankRowAddrW: Int = 12,         // rows per bank = 2560 -> 12-bit row address
  pprsLatency: Int = 4,           // PPRS pipeline latency
  outUnitsPerBeat: Int = 2,       // 2 × 96B egress units per beat
  unitBytes: Int = 96,            // bytes per egress unit
  maxNewPktPerCycle: Int = 3,     // max new packets per cycle (input)
  ctxPerPort: Int = 3,            // packet context slots per port
  readQueueDepth: Int = 64,       // pending-read FIFO depth
  reorderDepth: Int = 64,         // ReorderQueue depth
  bufferSizeKB: Int = 880,        // main buffer capacity
  bufAddrWidth: Int = 17,         // buffer address width
  macHeaderSize: Int = 8,         // 8B MAC header (4B TS + 4B reserved)
  minPktSize: Int = 64,           // min packet size incl. MAC header
  maxPfcPriority: Int = 8,        // PFC priority levels
  osaCount: Int = 2,              // OSA instances sharing pipeline (2-4)
  loopRateFixed: Int = 30,        // loopback rate limit, fixed-point 8 (30/8 = 3.75 seg/c = 300 Gbps)
  loopTokenCap: Int = 24,         // loopback token bucket depth (segments)
  loopBankCount: Int = 8,         // loopback memory banks (TP) per port
  loopBankWidthBytes: Int = 32,   // loopback bank word width
  loopMemDepth: Int = 128,        // loopback rows per bank (32 KB per port)
  pktOpenTimeout: Int = 4096,     // packet-context watchdog
  cellLockTimeout: Int = 4096,    // arbiter lock watchdog
) {
  def bufferEntries: Int = bufferSizeKB * 1024 / 8          // 112640
  def rowsPerBank: Int = bufferEntries / banks              // 2560
  def outSegPerBeat: Int = outUnitsPerBeat * unitBytes / 8  // 24
  def unitSegs: Int = unitBytes / 8                         // 12
  def ctxPool: Int = portCount * ctxPerPort                 // 24
  def loopRate0: Int = loopRateFixed
  def loopRate1: Int = loopRateFixed
  def loopBankWidthB: Int = loopBankWidthBytes * 8          // 256 bits
  def pprsPorts: Int = 16                                   // PreParser port count (>= portCount)
  def pprsTcamDepth: Int = 16
}
