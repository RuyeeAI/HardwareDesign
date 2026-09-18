package em

import chisel3._
import chisel3.util._
import BaseCbb.memory._

// ===========================================================================
// EM（Exact Match）表模块 —— 流水线版（指纹过滤 + 单实例 KT）
//
// 对外语义：
//   io.key : Decoupled（有 ready，反压不丢包）；io.wr : Decoupled；io.rsp : hit + ad
//   吞吐   : 命中路径 II=1（CrcHardwired 下）；svc 抢读端口时插入时隙气泡。
//
// ---------------------------------------------------------------------------
// 查找流程（三层过滤，只有指纹命中才读 KT）
// ---------------------------------------------------------------------------
//   S0 接收   key → CRC 哈希；哈希低位切出桶索引 idx[b]，桶索引**之上**的 fpW 位当指纹 fp
//   r1        idx 驱动 HT 全 bank 并行读（一次读出一整桶 ways 条 {fp, ktPtr}）
//   d1        · 桶内各条指纹与输入 fp **并行比对**（纯组合，在 HT 回读数据上做）
//             · 指纹命中的那一路（至多一路，见插入规则）用它的 ktPtr 发起**一次** KT 读
//             · OVFC 命中优先：命中时改用 OVFC 里的 ktPtr（OVFC 用全 key 比较，精确）
//   d2        · 只有这一条 KT 的 Full Key 参与比较（**不是多路 KT 一起比**）
//             · Full Key 相等 且 指纹命中位有效 → 命中；否则 miss
//   d3        AD 回读 → rsp
//
// 为什么"至多一路命中指纹" —— 插入规则（EmLayout 有完整说明）：
//   插入 K 时，只要 K 的候选槽位里已有另一条相同指纹的条目，本次插入就不进 HT
//   （落 OVFC 或 insFail，计 fpClash）。该关系对称，所以插入时查一次即可永久成立。
//   于是查找/维护的指纹命中掩码至多 1 位 → 只读 1 路 KT 就是精确的。
//
// 代价对比：原"读多路 KT 并行比全 key"需要 ktBanks 个 keyW 宽比较器 + ktBanks 路 KT 端口；
//   现在只需 ktBanks 个 fpW 宽比较器 + 1 个 keyW 比较器 + **1 路 KT 端口**。
//
// ---------------------------------------------------------------------------
// svc 共享访问引擎（维护口 / 自学习插入 / 老化动作）
// ---------------------------------------------------------------------------
//   优先级：老化动作 > wr > 自学习插入
//   每次操作至多 2 个"时隙"：① 读 HT 桶（+AgeTable 查询）② 指纹命中时读那 1 条 KT
//   时隙来源：① 流水线自然空档（零代价）② 等够 slotWaitMax 拍 → 反压上游并冻结流水线 1 拍
//   写不需要时隙：HT/KT/AD 都是 TP（同拍 1 读 + 1 写）
// ===========================================================================

object EmOp {
  val add = 0.U(2.W)   // 插入（已存在则覆盖动作数据）
  val del = 1.U(2.W)   // 删除
  val upd = 2.U(2.W)   // 更新（不存在则记失败）
}

class EmWrCmd(l: EmLayout) extends Bundle {
  val op  = UInt(2.W)
  val key = UInt(l.keyW.W)
  val ad  = UInt(l.adW.W)
}

class EmStatus(l: EmLayout) extends Bundle {
  val entries   = UInt(log2Ceil(l.p.htDepth * l.p.htWays + l.p.ovfcDepth + 1).W)
  val insert    = UInt(16.W)
  val insFail   = UInt(16.W)
  val delete    = UInt(16.W)
  val learn     = UInt(16.W)
  val ageDrop   = UInt(16.W)
  val learnDrop = UInt(16.W)   // 转发 CAM 满导致漏学的次数
  val fpClash   = UInt(16.W)   // 因"候选槽位里已有同指纹条目"而无法进 HT 的次数（落 OVFC/insFail）
  val ovfcUse   = UInt(math.max(1, log2Ceil(l.p.ovfcDepth + 1)).W)
  val fwdUse    = UInt(math.max(1, log2Ceil(l.p.learnFwdDepth + 1)).W)
  // KT / AD 空闲条目总数（用来验证"老化后资源确实被回收"）
  val ktFree    = UInt(log2Ceil(l.p.ktDepth + 1).W)
  val adFree    = UInt(log2Ceil(l.p.adDepth + 1).W)
  val lkBusy    = Bool()       // 查找流水线非空
  val mtBusy    = Bool()       // svc 引擎非空闲
}

class EmIo(l: EmLayout) extends Bundle {
  // ---- 查表口（Decoupled：满则反压上游，不再丢请求）----
  val key = Flipped(Decoupled(UInt(l.keyW.W)))
  val rsp = Output(Valid(UInt(l.rspW.W)))
  // ---- 维护口（控制面 / 软件下发）----
  val wr = Flipped(Decoupled(new EmWrCmd(l)))
  // ---- 自学习 / 老化运行时开关 ----
  val learnAd = Input(UInt(l.adW.W))   // 与 io.key 同拍对齐（随包携带）
  val learnEn = Input(Bool())
  val ageEn   = Input(Bool())
  // ---- 存储初始化 ----
  val memInit     = Input(Bool())
  val memInitDone = Output(Bool())
  // ---- CRC 配置（仅 CrcRuntime 模式生效）----
  val crcPoly = Input(UInt(l.crcW.W))
  val crcInit = Input(UInt(l.crcW.W))
  val crcXor  = Input(UInt(l.crcW.W))
  // ---- 状态与统计 ----
  val status = Output(new EmStatus(l))
}

class ExactMatch(params: EmParams) extends Module {
  val l = EmLayout(params)
  val io = IO(new EmIo(l))

  val numBanks = l.numBanks
  val ways     = l.ways
  val ktBanks  = l.ktBanks           // 一次查找的候选槽位数（不再是 KT 物理 bank 数）
  val keyW     = l.keyW
  val adW      = l.adW
  val useKt    = params.useKt
  val useAd    = params.useAd
  val fpW      = math.max(1, l.fpW)
  val ovfcEn   = l.ovfcEn
  val ovfcD    = params.ovfcDepth
  val agingOn  = l.agingOn
  val sweepOn  = l.sweepOn
  val learnOn  = params.learning.isDefined
  val fwdD     = params.learnFwdDepth
  val timeout  = params.aging.map(_.timeout).getOrElse(0)

  private val fwdCntW   = math.max(1, log2Ceil(fwdD + 1))
  private val ovfcCntW1 = math.max(1, log2Ceil(ovfcD + 1))
  private val useD3     = useKt && useAd

  // =========================================================================
  // 存储实例
  // =========================================================================
  private def memCfg(name: String, width: Int, depth: Int): Memory = Memory(
    name       = name,
    dataType   = UInt(width.W),
    depth      = depth,
    memoryType = MemoryAccessType.TP,
    protect    = params.memProtect,
    flopIn     = false,
    flopOut    = false,
    CheckIn    = false,
    CheckOut   = false,
    RsAccess   = false,
    initValue  = MemoryInitType.AllZero
  )

  private def driveAux(m: TpMemoryWrap3): Unit = {
    m.io.cpu.we    := false.B
    m.io.cpu.re    := false.B
    m.io.cpu.addr  := 0.U
    m.io.cpu.wdata := 0.U
    m.io.cpuCfg.idleCycleTh0 := 0.U
    m.io.dfx.init  := io.memInit
    m.io.dfx.injCorrEn := false.B
    m.io.dfx.injUerrEn := false.B
  }

  val htMems = Seq.tabulate(numBanks) { b =>
    // 宽度必须是**整桶字** htWordW = htPayW*ways：HT 一次读一整桶，ways 条 payload 拼成一个字
    val m = Module(new TpMemoryWrap3(memCfg(s"EmHt$b", l.htWordW, l.bankDepth)))
    driveAux(m); m
  }
  // KT 单实例（每次查找只读 1 路）
  val ktMem: Option[TpMemoryWrap3] =
    if (useKt) {
      val m = Module(new TpMemoryWrap3(memCfg("EmKt", l.ktEntryW, l.ktDepthReal)))
      driveAux(m); Some(m)
    } else None
  val adMem: Option[TpMemoryWrap3] =
    if (useAd) {
      val m = Module(new TpMemoryWrap3(memCfg("EmAd", l.adW, params.adDepth)))
      driveAux(m); Some(m)
    } else None

  // 流水线按"发起读的下一拍捕获"硬编码（rdLat=1）。换 Memory 配置（flopIn/flopOut/CheckOut）
  // 会改变读延迟，届时必须同步插入流水级，否则数据与 metadata 会**静默错位**。
  // 这里在 elaboration 期直接拦住，不靠仿真碰运气。
  private def latOf(w: Int, d: Int): Int = {
    val m = memCfg("latProbe", w, d)
    m.latency + (if (m.CheckOut) 1 else 0)
  }
  require(latOf(l.htWordW, l.bankDepth) == 1, "HT 读延迟必须为 1 拍，当前为 " + latOf(l.htWordW, l.bankDepth))
  require(latOf(l.ktEntryW, l.ktDepthReal) == 1, "KT 读延迟必须为 1 拍，当前为 " + latOf(l.ktEntryW, l.ktDepthReal))
  require(latOf(l.adW, params.adDepth) == 1, "AD 读延迟必须为 1 拍，当前为 " + latOf(l.adW, params.adDepth))

  private val memReady = htMems.map(_.io.dfx.initDone).reduce(_ && _) &&
    ktMem.map(_.io.dfx.initDone).getOrElse(true.B) &&
    adMem.map(_.io.dfx.initDone).getOrElse(true.B) && !io.memInit
  io.memInitDone := memReady

  // =========================================================================
  // CRC / 哈希
  // =========================================================================
  val useSerial = params.crc.isInstanceOf[CrcRuntime]
  val lkCrc = if (useSerial) Some(Module(new CrcSerial(l.crcW, keyW, params.crc.refin, params.crc.refout))) else None
  val svCrc = if (useSerial) Some(Module(new CrcSerial(l.crcW, keyW, params.crc.refin, params.crc.refout))) else None
  Seq(lkCrc, svCrc).flatten.foreach { m =>
    m.io.start := false.B
    m.io.din   := 0.U
    m.io.poly  := io.crcPoly
    m.io.init  := io.crcInit
    m.io.xor   := io.crcXor
  }

  def hardHash(d: UInt): UInt = {
    val c = params.crc
    Crc.hardwired(d, l.crcW, c.poly, c.init, c.xorout, c.refin, c.refout)
  }
  def slices(h: UInt): Vec[UInt] =
    VecInit((0 until numBanks).map(b => h(math.min(l.crcW - 1, (b + 1) * l.idxW - 1), b * l.idxW)))

  // =========================================================================
  // 时间基准（老化）
  // =========================================================================
  val now = if (agingOn) RegInit(0.U(l.ageW1.W)) else WireDefault(0.U(1.W))
  if (agingOn) {
    val tick = RegInit(0.U(32.W))
    when(tick === (params.aging.get.tickDiv - 1).U) { tick := 0.U; now := now + 1.U }
    .otherwise { tick := tick + 1.U }
  }
  def expired(ts: UInt): Bool =
    if (agingOn) (now - ts) >= timeout.U(l.ageW1.W) else false.B

  // =========================================================================
  // 老化信息表 / 空闲栈 / 转发 CAM
  // =========================================================================
  val ageTab = Module(new AgeTable(l, timeout))

  val ktFree: Option[FreeList] = if (useKt) Some(Module(new FreeList(l.ktDepthReal))) else None
  val adFree: Option[FreeList] = if (useAd) Some(Module(new FreeList(params.adDepth))) else None
  ktFree.foreach { f => f.io.alloc := false.B; f.io.free := false.B; f.io.faddr := 0.U }
  adFree.foreach { f => f.io.alloc := false.B; f.io.free := false.B; f.io.faddr := 0.U }

  val fwd = if (learnOn) Some(Module(new ForwardCam(fwdD, keyW, adW))) else None

  // =========================================================================
  // OVFC：HT 溢出 TCAM（寄存器阵列，全 key 并行比较，精确）
  // =========================================================================
  val ovfcV = if (ovfcEn) RegInit(VecInit(Seq.fill(ovfcD)(false.B))) else null
  val ovfcC = if (ovfcEn) RegInit(VecInit(Seq.fill(ovfcD)(false.B))) else null  // claim
  val ovfcK = if (ovfcEn) Reg(Vec(ovfcD, UInt(keyW.W))) else null
  val ovfcP = if (ovfcEn) Reg(Vec(ovfcD, UInt(math.max(1, l.ovfcPayW).W))) else null
  val ovfcT = if (ovfcEn) Reg(Vec(ovfcD, UInt(l.ageW1.W))) else null
  val ovfcUseCnt = if (ovfcEn) RegInit(0.U(ovfcCntW1.W)) else 0.U(1.W)

  /** OVFC 并行匹配（未启用时恒 miss）；序号小的优先 */
  def ovfcMatch(k: UInt): (Bool, UInt) =
    if (!ovfcEn) (false.B, 0.U(1.W))
    else {
      val hit = VecInit((0 until ovfcD).map(i => ovfcV(i) && !ovfcC(i) && ovfcK(i) === k))
      (hit.asUInt.orR, PriorityEncoder(hit))
    }
  def ovfcPay(i: UInt): UInt = if (ovfcEn) ovfcP(i) else 0.U(1.W)

  val ovfcHasFree = if (ovfcEn) ovfcUseCnt =/= ovfcD.U else false.B
  val ovfcFreeSel = if (ovfcEn) PriorityEncoder(VecInit((0 until ovfcD).map(i => !ovfcV(i) && !ovfcC(i)))) else 0.U(1.W)
  def ovfcExp(i: UInt): Bool = if (ovfcEn && agingOn) expired(ovfcT(i)) else false.B

  // =========================================================================
  // 统计
  // =========================================================================
  val cntInsert  = RegInit(0.U(16.W))
  val cntInsFail = RegInit(0.U(16.W))
  val cntDelete  = RegInit(0.U(16.W))
  val cntLearn   = RegInit(0.U(16.W))
  val cntAgeDrop = RegInit(0.U(16.W))
  val cntLrnDrop = RegInit(0.U(16.W))
  val cntFpClash = RegInit(0.U(16.W))
  val entryCnt   = RegInit(0.U(log2Ceil(params.htDepth * ways + ovfcD + 1).W))

  // =========================================================================
  // svc 状态与时隙（先声明，流水线推进要用 adv）
  // =========================================================================
  val S_IDLE  = 0.U(4.W); val S_HASH = 1.U(4.W); val S_HTREQ = 2.U(4.W); val S_HTW = 3.U(4.W)
  val S_KTREQ = 4.U(4.W); val S_KTW  = 5.U(4.W); val S_DEC   = 6.U(4.W); val S_ALLOC = 7.U(4.W)
  val S_WR    = 8.U(4.W); val S_FREE = 9.U(4.W); val S_AGFR  = 10.U(4.W)
  // S_HTD：HT 数据"决策"拍 —— 必须独立于 S_HTW，因为 sPay 是 S_HTW 当拍末才写入的，
  // 在 S_HTW 当拍就用 sFpHit/sFpSel（寄存器版）判断，拿到的是**上一轮**的桶数据。
  val S_HTD   = 11.U(4.W)
  val sState = RegInit(S_IDLE)

  // =========================================================================
  // 查找流水线寄存器（metadata 影子：r1 → d1 → d2 → d3）
  // =========================================================================
  val acqV   = RegInit(false.B)
  val acqKey = Reg(UInt(keyW.W))
  val acqAd  = Reg(UInt(adW.W))
  val acqIdx = Reg(Vec(numBanks, UInt(l.idxW.W)))
  val acqFp  = Reg(UInt(fpW.W))
  val acqOvH = Reg(Bool())
  val acqOvS = Reg(UInt(ovfcCntW1.W))
  val acqOvP = Reg(UInt(math.max(1, l.ovfcPayW).W))
  val crcRun = RegInit(false.B)

  val (lkOvHitC, lkOvSelC) = ovfcMatch(io.key.bits)

  // ---- r1：驱动 HT 读 ----
  val r1V   = RegInit(false.B)
  val r1Key = Reg(UInt(keyW.W))
  val r1Ad  = Reg(UInt(adW.W))
  val r1Idx = Reg(Vec(numBanks, UInt(l.idxW.W)))
  val r1Fp  = Reg(UInt(fpW.W))
  val r1OvH = Reg(Bool())
  val r1OvS = Reg(UInt(ovfcCntW1.W))
  val r1OvP = Reg(UInt(math.max(1, l.ovfcPayW).W))

  // ---- d1：HT 数据拍（指纹比对 + 发起唯一一次 KT 读）----
  val d1V     = RegInit(false.B)
  val d1Key   = Reg(UInt(keyW.W))
  val d1Ad    = Reg(UInt(adW.W))
  val d1Idx   = Reg(Vec(numBanks, UInt(l.idxW.W)))
  val d1Fp    = Reg(UInt(fpW.W))
  val d1OvH   = Reg(Bool())
  val d1OvS   = Reg(UInt(ovfcCntW1.W))
  val d1OvP   = Reg(UInt(math.max(1, l.ovfcPayW).W))
  val d1Val   = Reg(Vec(ktBanks, Bool()))        // HT 各候选的 valid（来自 AgeTable）
  // 注意：指纹比对结果**不设寄存器**。d1Val 与本拍的比对是同一个边沿更新的，
  // 若把比对结果存在 d1FpOk 里，捕获到的会是"上一拍 d1Val（上一个请求）"的比对结果。
  // 直接在 d1 拍组合算好、捕获进 d2。

  // ---- d2：KT 数据拍（useKt，只比这一条 Full Key）/ AD 数据拍（!useKt && useAd）----
  val d2V     = RegInit(false.B)
  val d2Key   = Reg(UInt(keyW.W))
  val d2Ad    = Reg(UInt(adW.W))
  val d2Idx   = Reg(Vec(numBanks, UInt(l.idxW.W)))
  val d2OvH   = Reg(Bool())
  val d2OvS   = Reg(UInt(ovfcCntW1.W))
  val d2OvP   = Reg(UInt(math.max(1, l.ovfcPayW).W))
  val d2Val   = Reg(Vec(ktBanks, Bool()))
  val d2FpOk  = Reg(Bool())
  val d2FpSel = Reg(UInt(l.slotW.W))
  val d2Hit   = Reg(Bool())                      // !useKt && useAd 时的命中
  val d2FwH   = Reg(Bool())
  val d2FwAd  = Reg(UInt(adW.W))

  // ---- d3：AD 数据拍（useKt && useAd）----
  val d3V    = RegInit(false.B)
  val d3Hit  = Reg(Bool())
  val d3FwH  = Reg(Bool())
  val d3FwAd = Reg(UInt(adW.W))

  val htRdUsed = r1V
  val ktRdUsed = useKt.B && d1V
  val adRdUsed = useAd.B && (if (useKt) d2V else d1V)
  val pipeFree = !htRdUsed && !ktRdUsed && !adRdUsed

  // 时隙等待计时
  val svcWait  = RegInit(0.U(math.max(1, log2Ceil(params.slotWaitMax + 1)).W))
  val svcWantC = (sState === S_HTREQ) || (sState === S_KTREQ)
  val svcForce = svcWantC && svcWait === params.slotWaitMax.U
  val svcOwns  = svcWantC && (pipeFree || svcForce)
  val adv      = !svcOwns
  when(svcOwns) { svcWait := 0.U }
  .elsewhen(svcWantC) { when(svcWait =/= params.slotWaitMax.U) { svcWait := svcWait + 1.U } }

  // =========================================================================
  // AgeTable 扫描 → claim（同时只处理一个）
  // =========================================================================
  val agPend  = RegInit(false.B)
  val agOv    = RegInit(false.B)
  val agBk    = Reg(UInt(l.bankW.W))
  val agIdx   = Reg(UInt(l.idxW.W))
  val agWy    = Reg(UInt(l.wayW.W))
  val agOvSel = Reg(UInt(ovfcCntW1.W))
  val agSlot  = agBk * ways.U + agWy

  val ovScIdx = RegInit(0.U(ovfcCntW1.W))
  val scanOn  = (if (agingOn && sweepOn) io.ageEn && memReady else false.B)
  val ovScHit = if (ovfcEn) scanOn && !agPend && ovfcV(ovScIdx) && !ovfcC(ovScIdx) && ovfcExp(ovScIdx) else false.B
  if (ovfcEn) {
    when(scanOn && !agPend) { ovScIdx := Mux(ovScIdx === (ovfcD - 1).U, 0.U, ovScIdx + 1.U) }
  }

  ageTab.io.now      := now
  ageTab.io.scanEn   := scanOn
  ageTab.io.scanHold := agPend
  val htScHit = ageTab.io.scanHit

  ageTab.io.clmEn  := htScHit && !agPend
  ageTab.io.clmBk  := ageTab.io.scanBk
  ageTab.io.clmIdx := ageTab.io.scanIdx
  ageTab.io.clmWy  := ageTab.io.scanWy

  when(htScHit && !agPend) {
    agPend := true.B; agOv := false.B
    agBk := ageTab.io.scanBk; agIdx := ageTab.io.scanIdx; agWy := ageTab.io.scanWy
  }.elsewhen(ovScHit) {
    agPend := true.B; agOv := true.B; agOvSel := ovScIdx
    if (ovfcEn) { ovfcC(ovScIdx) := true.B }
  }

  // =========================================================================
  // svc 寄存器
  // =========================================================================
  val T_WR = 0.U(2.W); val T_LRN = 1.U(2.W); val T_AGE = 2.U(2.W)
  val sTask  = Reg(UInt(2.W))
  val sKey   = Reg(UInt(keyW.W))
  val sAd    = Reg(UInt(adW.W))
  val sOp    = Reg(UInt(2.W))
  val sIdx   = Reg(Vec(numBanks, UInt(l.idxW.W)))
  val sFp    = Reg(UInt(fpW.W))
  val sPay   = Reg(Vec(ktBanks, UInt(l.htPayW.W)))
  val sVal   = Reg(Vec(ktBanks, Bool()))
  val sClm   = Reg(Vec(ktBanks, Bool()))
  val sKtE   = Reg(UInt(l.ktEntryW.W))
  val sFound = RegInit(false.B)
  val sUseOv = RegInit(false.B)
  val sOvSel = Reg(UInt(ovfcCntW1.W))
  val sSlot  = Reg(UInt(l.slotW.W))         // 扁平槽位 b*ways+w
  val sKtPtr = Reg(UInt(l.ktPtrW.W))        // KT 全局索引
  val sAdPtr = Reg(UInt(l.adPtrW.W))
  val sBest  = Reg(UInt(l.bankW.W))
  val sFreeW = Reg(UInt(l.wayW.W))

  /** 维护：桶内与待查 key 同指纹且 valid 的槽位掩码（插入规则保证至多 1 位） */
  val sFpMatch = if (useKt) VecInit((0 until ktBanks).map(s => sVal(s) && l.htFp(sPay(s)) === sFp))
                 else VecInit(Seq(false.B))
  val sFpHit   = if (useKt) sFpMatch.asUInt.orR else false.B
  val sFpSel   = PriorityEncoder(sFpMatch)

  // =========================================================================
  // 读地址驱动（流水线与 svc 二选一）
  // =========================================================================
  // HT：r1 拍发起（每 bank 独立一次）
  for (b <- 0 until numBanks) {
    htMems(b).io.lgc.raddr := Mux(svcOwns, sIdx(b), r1Idx(b))
    htMems(b).io.lgc.re    := Mux(svcOwns, false.B, htRdUsed)
  }
  ageTab.io.qIdx := VecInit((0 until numBanks).map(b => Mux(svcOwns, sIdx(b), r1Idx(b))))

  /** 当拍 HT 回读数据里、槽位 s 对应的 payload（组合） */
  val htRdataV = VecInit(htMems.map(_.io.lgc.rdata))
  def htNow(s: UInt): UInt = {
    val word = htRdataV(l.slotBank(s)).asTypeOf(Vec(ways, UInt(l.htPayW.W)))
    word(l.slotWay(s))
  }

  // ---- d1：指纹并行比对（纯组合），挑出唯一命中路 ----
  val d1FpMatch = if (useKt) VecInit((0 until ktBanks).map(s => d1Val(s) && l.htFp(htNow(s.U(l.slotW.W))) === d1Fp))
                  else VecInit(Seq(false.B))
  val d1FpSelC  = PriorityEncoder(d1FpMatch)
  val d1FpOkC   = d1FpMatch.asUInt.orR

  // ---- KT 读地址：OVFC 命中优先，其次指纹命中的那一路（只读 1 路）----
  val ktRdAddrPipe: UInt =
    if (!useKt) 0.U(l.ktPtrW.W)
    else Mux(d1OvH, d1OvP, Mux(d1FpOkC, l.htKtPtr(htNow(d1FpSelC)), 0.U(l.ktPtrW.W)))
  // svc 的 KT 读：维护 → 指纹命中槽位；老化 → 被 claim 的槽位
  val svcKtRAddr: UInt = if (useKt) l.htKtPtr(sPay(sSlot)) else 0.U(l.ktPtrW.W)

  ktMem.foreach { m =>
    m.io.lgc.raddr := Mux(svcOwns, svcKtRAddr, ktRdAddrPipe)
    m.io.lgc.re    := Mux(svcOwns, (sState === S_KTREQ), ktRdUsed)
  }

  // =========================================================================
  // 比较级（cm 拍）：useKt 时在 d2，否则在 d1
  // =========================================================================
  val cmV     = if (useKt) d2V else d1V
  val cmKey   = if (useKt) d2Key else d1Key
  val cmAd    = if (useKt) d2Ad else d1Ad
  val cmIdx   = if (useKt) d2Idx else d1Idx
  val cmVal   = if (useKt) d2Val else d1Val
  val cmOvH   = if (useKt) d2OvH else d1OvH
  val cmOvS   = if (useKt) d2OvS else d1OvS
  val cmOvP   = if (useKt) d2OvP else d1OvP

  // 转发 CAM 比对：与 miss 判定同拍 → 背靠背的下一个请求立刻可见
  fwd.foreach(_.io.lookKey := cmKey)
  val fwdHit = if (learnOn) fwd.get.io.hit else false.B
  val fwdAd  = if (learnOn) fwd.get.io.hitAd else 0.U(adW.W)

  // ---- 表内命中判定 ----
  //   useKt : 只有指纹命中的那一条 KT 参与 Full Key 比较（**不是多路 KT 一起比**）
  //   !useKt: key 内联在 HT payload 里，桶内并行比较
  val cmHitM = if (!useKt) VecInit((0 until ktBanks).map(s => cmVal(s) && (l.htKey(htNow(s.U(l.slotW.W))) === cmKey)))
               else VecInit(Seq(false.B))
  val cmSel  = PriorityEncoder(cmHitM)
  // !useKt 时没有指纹（key 直接内联），fpOk 恒真、命中槽位取内联比较的结果
  val cmFpOk  = if (useKt) d2FpOk else true.B
  val cmFpSel = if (useKt) d2FpSel else cmSel
  val cmHit: Bool = if (useKt) (cmFpOk && (l.ktKey(ktMem.get.io.lgc.rdata) === cmKey))
                    else cmHitM.asUInt.orR
  val tblHit = Mux(cmOvH, true.B, cmHit)

  /** 命中条目的 payload / KT 条目（供取 AD 指针）；useKt 时 OVFC 命中也读的是这一路 KT */
  val srcEnt: UInt =
    if (useKt) ktMem.get.io.lgc.rdata
    else Mux(cmOvH, cmOvP, htNow(cmSel))
  val tblAdPtr = (if (useAd) (if (useKt) l.ktAdPtr(srcEnt) else l.htAdPtr(srcEnt)) else 0.U(1.W))
  val tblAdVal = (if (useAd) 0.U(1.W) else (if (useKt) l.ktAd(srcEnt) else l.htAd(srcEnt)))
  val hit = tblHit || fwdHit

  // 命中刷新（只写时间戳，不写 valid → 结构上不可能复活已老化条目）
  val rfOn = if (agingOn) cmV && io.ageEn && tblHit else false.B
  val rfBk = l.slotBank(cmFpSel)
  ageTab.io.rfEn  := rfOn && !cmOvH
  ageTab.io.rfBk  := rfBk
  ageTab.io.rfIdx := cmIdx(rfBk)
  ageTab.io.rfWy  := l.slotWay(cmFpSel)
  if (ovfcEn) {
    when(rfOn && cmOvH) { ovfcT(cmOvS) := now }
  }

  // 自学习：判定 miss 的当拍压入转发 CAM
  val learnReq = learnOn.B && cmV && io.learnEn && !hit && memReady
  if (learnOn) {
    fwd.get.io.push    := learnReq && !fwd.get.io.full
    fwd.get.io.pushKey := cmKey
    fwd.get.io.pushAd  := cmAd
    when(learnReq && fwd.get.io.full) { cntLrnDrop := cntLrnDrop + 1.U }
  }

  // =========================================================================
  // AD 读（cm 拍发起；svc 不读 AD）
  // =========================================================================
  adMem.foreach { m =>
    m.io.lgc.raddr := Mux(fwdHit, 0.U, tblAdPtr)
    m.io.lgc.re    := Mux(svcOwns, false.B, adRdUsed)
  }

  // =========================================================================
  // 流水线推进（svcOwns 时整体冻结）
  // =========================================================================
  io.key.ready := adv && (if (useSerial) !crcRun else true.B)

  // r1 无条件跟随 acq；acq 被提交后清零（同拍又接收新请求会再次置位 → 背靠背不丢拍）。
  // ⚠️ 不能在 when(acqV) 里写 r1V := ...，否则 acqV=0 的那些拍 r1V 清不掉，流水线会卡死。
  when(adv) {
    r1V := acqV; r1Key := acqKey; r1Ad := acqAd; r1Idx := acqIdx; r1Fp := acqFp
    r1OvH := acqOvH; r1OvS := acqOvS; r1OvP := acqOvP
    when(acqV) { acqV := false.B }
  }
  when(io.key.valid && io.key.ready) {
    acqKey := io.key.bits
    acqAd  := io.learnAd
    acqOvH := lkOvHitC; acqOvS := lkOvSelC; acqOvP := ovfcPay(lkOvSelC)
    if (useSerial) {
      lkCrc.get.io.start := true.B; lkCrc.get.io.din := io.key.bits; crcRun := true.B
    } else {
      val h = hardHash(io.key.bits)
      acqIdx := slices(h); acqFp := l.fpOf(h); acqV := true.B
    }
  }
  if (useSerial) {
    when(crcRun && lkCrc.get.io.done) {
      val h = lkCrc.get.io.out
      acqIdx := slices(h); acqFp := l.fpOf(h); acqV := true.B; crcRun := false.B
    }
  }

  when(adv) {
    // r1 → d1：metadata 影子 + 指纹比对结果（AgeTable 的 valid 位图在 r1 拍组合读出）
    d1V := r1V; d1Key := r1Key; d1Ad := r1Ad; d1Idx := r1Idx; d1Fp := r1Fp
    d1OvH := r1OvH; d1OvS := r1OvS; d1OvP := r1OvP
    for (b <- 0 until numBanks) {
      for (w <- 0 until ways) { d1Val(b * ways + w) := ageTab.io.qEnt(b)(w)(0) }
    }
    // d1 → d2
    if (useKt) {
      d2V := d1V; d2Key := d1Key; d2Ad := d1Ad; d2Idx := d1Idx
      d2OvH := d1OvH; d2OvS := d1OvS; d2OvP := d1OvP; d2Val := d1Val
      // 用 d1 拍当拍的组合比对结果（此时 d1Val 已是本请求的 valid 位）
      d2FpOk := d1FpOkC; d2FpSel := d1FpSelC
    } else if (useAd) {
      d2V := d1V; d2Hit := tblHit; d2FwH := fwdHit; d2FwAd := fwdAd
    }
    // d2 → d3
    if (useD3) {
      d3V := d2V; d3Hit := tblHit; d3FwH := fwdHit; d3FwAd := fwdAd
    }
  }

  if (useAd) {
    if (useKt) {
      io.rsp.valid := d3V
      io.rsp.bits  := Cat(d3Hit || d3FwH, Mux(d3FwH, d3FwAd, adMem.get.io.lgc.rdata))
    } else {
      io.rsp.valid := d2V
      io.rsp.bits  := Cat(d2Hit || d2FwH, Mux(d2FwH, d2FwAd, adMem.get.io.lgc.rdata))
    }
  } else {
    io.rsp.valid := cmV
    io.rsp.bits  := Cat(hit, Mux(fwdHit, fwdAd, tblAdVal))
  }

  // =========================================================================
  // svc 主状态机
  // =========================================================================
  val fwdPending = if (learnOn) !fwd.get.io.empty else false.B
  io.wr.ready := (sState === S_IDLE) && !agPend

  def useOvNow(k: UInt): (Bool, UInt) = if (ovfcEn) ovfcMatch(k) else (false.B, 0.U(1.W))

  /** 启动一次维护/学习任务：命中 OVFC 就直接用它的 KT 指针，否则先读 HT 桶 */
  def startTask(k: UInt, a: UInt, op: UInt, task: UInt): Unit = {
    sKey := k; sAd := a; sOp := op; sTask := task
    val ovm = useOvNow(k)
    sUseOv := ovm._1; sOvSel := ovm._2
    if (useSerial) { svCrc.get.io.start := true.B; svCrc.get.io.din := k; sState := S_HASH }
    else {
      val h = hardHash(k)
      sIdx := slices(h); sFp := l.fpOf(h)
      when(ovm._1) {
        sKtPtr := ovfcPay(ovm._2)
        sState := Mux(useKt.B, S_KTREQ, S_DEC)
      }.otherwise {
        sState := S_HTREQ
      }
    }
  }

  when(sState === S_IDLE) {
    when(agPend) {
      sTask := T_AGE
      sUseOv := agOv
      sOvSel := agOvSel
      when(agOv) {
        sKtPtr := ovfcPay(agOvSel)
        sState := Mux(useKt.B, S_KTREQ, S_AGFR)
      }.otherwise {
        for (b <- 0 until numBanks) { sIdx(b) := agIdx }
        sState := S_HTREQ
      }
    }.elsewhen(io.wr.valid) {
      startTask(io.wr.bits.key, io.wr.bits.ad, io.wr.bits.op, T_WR)
    }.elsewhen(if (learnOn) fwdPending else false.B) {
      if (learnOn) { startTask(fwd.get.io.headKey, fwd.get.io.headAd, EmOp.add, T_LRN) }
    }
  }
  if (useSerial) {
    when(sState === S_HASH) {
      when(svCrc.get.io.done) {
        val h = svCrc.get.io.out
        sIdx := slices(h); sFp := l.fpOf(h)
        val ovm = useOvNow(sKey)
        sUseOv := ovm._1; sOvSel := ovm._2
        when(ovm._1) { sKtPtr := ovfcPay(ovm._2); sState := Mux(useKt.B, S_KTREQ, S_DEC) }
        .otherwise { sState := S_HTREQ }
      }
    }
  }

  when(svcOwns && sState === S_HTREQ) {
    sState := S_HTW
    for (b <- 0 until numBanks) {
      for (w <- 0 until ways) {
        sVal(b * ways + w) := ageTab.io.qEnt(b)(w)(0)
        sClm(b * ways + w) := ageTab.io.qEnt(b)(w)(1 + l.ageW)
      }
    }
  }
  when(svcOwns && sState === S_KTREQ) { sState := S_KTW }

  when(sState === S_HTW) {
    for (b <- 0 until numBanks) {
      val rd = htMems(b).io.lgc.rdata.asTypeOf(Vec(ways, UInt(l.htPayW.W)))
      for (w <- 0 until ways) { sPay(b * ways + w) := rd(w) }
    }
    sState := S_HTD        // 决策放到下一拍，此时 sPay 已是本桶数据
  }

  // ---- S_HTD：用刚捕获的桶数据（sPay/sVal）做判断 ----
  when(sState === S_HTD) {
    when(sTask === T_AGE) {
      // 老化：拿被 claim 那一槽的 payload → KT 指针
      sSlot  := agSlot
      sKtPtr := (if (useKt) l.htKtPtr(sPay(agSlot)) else 0.U(l.ktPtrW.W))
      sState := Mux(useKt.B, S_KTREQ, S_AGFR)
    }.otherwise {
      // 维护：先看指纹有没有命中 —— 没命中说明 key 不在 HT，省掉这次 KT 读
      if (useKt) {
        when(sFpHit) { sSlot := sFpSel; sState := S_KTREQ }
        .otherwise   { sState := S_DEC }
      } else { sState := S_DEC }
    }
  }

  if (useKt) {
    when(sState === S_KTW) {
      sKtE := ktMem.get.io.lgc.rdata
      sState := Mux(sTask === T_AGE, S_AGFR, S_DEC)
    }
  }

  // ---- 维护判定 ----
  // 注意：htHitM 只能在 !useKt 时构造（l.htKey 带 require(!useKt)，elaboration 期会检查）。
  when(sState === S_DEC) {
    val found: Bool =
      if (useKt) sFpHit && (l.ktKey(sKtE) === sKey)   // 只比指纹命中的那一条 Full Key
      else VecInit((0 until ktBanks).map(s => sVal(s) && l.htKey(sPay(s)) === sKey)).asUInt.orR
    val sel: UInt = if (useKt) sFpSel else PriorityEncoder(
      VecInit((0 until ktBanks).map(s => sVal(s) && l.htKey(sPay(s)) === sKey)))
    when(sUseOv) {
      sFound := true.B
      if (useAd) {
        val a = if (useKt) l.ktAdPtr(sKtE)
                else if (ovfcEn) l.htAdPtr(ovfcP(sOvSel))
                else 0.U(1.W)
        sAdPtr := a
      }
      sState := Mux(sOp === EmOp.del, S_FREE, S_WR)
    }.elsewhen(found) {
      sFound := true.B
      if (useKt) { sKtPtr := l.htKtPtr(sPay(sFpSel)) }
      sSlot := sel
      if (useAd) {
        val a = if (useKt) l.ktAdPtr(sKtE) else l.htAdPtr(sPay(sel))
        sAdPtr := a
      }
      sState := Mux(sOp === EmOp.del, S_FREE, S_WR)
    }.elsewhen(sOp === EmOp.del) {
      sFound := false.B
      sState := S_IDLE
    }.elsewhen(sOp === EmOp.upd) {
      sFound := false.B
      cntInsFail := cntInsFail + 1.U
      sState := S_IDLE
    }.otherwise {
      sFound := false.B
      sState := S_ALLOC
    }
  }

  // ---- d-left 选路 + FreeList 分配 ----
  // 插入规则：候选槽位里已有同指纹条目（sFpHit）时，整个 HT 都不能放这条 key，
  // 否则后续查找会撞上别人的指纹 → 直接落 OVFC（OVFC 用全 key 比较，精确）。
  val allocWill = Wire(Bool())
  allocWill := false.B
  when(sState === S_ALLOC) {
    val fpClash = sFpHit
    when(fpClash) { cntFpClash := cntFpClash + 1.U }

    val occ = VecInit((0 until numBanks).map { b =>
      PopCount(VecInit((0 until ways).map(w => sVal(b * ways + w) || sClm(b * ways + w))).asUInt)
    })
    val minCnt  = occ.reduce((a, b) => Mux(a < b, a, b))
    val minMask = VecInit((0 until numBanks).map(b => occ(b) === minCnt))
    val tieBase: UInt = {
      if (numBanks == 1) 0.U(l.bankW.W)
      else params.dLeftTie match {
        case TiePolicy.Leftmost => 0.U(l.bankW.W)
        case TiePolicy.Random =>
          val lfsr = RegInit(1.U(16.W))
          lfsr := Cat(lfsr(14, 0), lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10))
          lfsr(math.max(1, log2Ceil(numBanks)) - 1, 0)
        case TiePolicy.RoundRobin =>
          val rr = RegInit(0.U(l.bankW.W))
          when(sState === S_WR && !sFound && !sUseOv) { rr := Mux(rr === (numBanks - 1).U, 0.U, rr + 1.U) }
          rr
      }
    }
    val bestBank =
      if (numBanks == 1) 0.U(l.bankW.W)
      else {
        val nb = numBanks.U((l.bankW + 1).W)
        val order = VecInit((0 until numBanks).map { k =>
          val s = tieBase +& k.U(l.bankW.W)
          Mux(s >= nb, s - nb, s)(l.bankW - 1, 0)
        })
        order(PriorityEncoder(VecInit(order.map(i => minMask(i)))))
      }
    val freeMask = VecInit((0 until ways).map(w => !(sVal(bestBank * ways.U + w.U) || sClm(bestBank * ways.U + w.U))))
    val htFree   = !fpClash && freeMask.asUInt.orR
    val htSlot   = bestBank * ways.U + PriorityEncoder(freeMask)

    sUseOv := !htFree
    sBest  := bestBank
    sFreeW := PriorityEncoder(freeMask)
    sSlot  := htSlot
    if (ovfcEn) { when(!htFree) { sOvSel := ovfcFreeSel } }

    allocWill := htFree || ovfcHasFree

    val ktOk = ktFree.map(_.io.ok).getOrElse(true.B)
    val adOk = adFree.map(_.io.ok).getOrElse(true.B)
    when(allocWill && ktOk && adOk) {
      ktFree.foreach(f => sKtPtr := f.io.addr)
      adFree.foreach(f => sAdPtr := f.io.addr)
      sState := S_WR
    }.otherwise {
      sState := S_IDLE
      cntInsFail := cntInsFail + 1.U
    }
  }
  ktFree.foreach(_.io.alloc := (sState === S_ALLOC) && allocWill)
  adFree.foreach(_.io.alloc := (sState === S_ALLOC) && allocWill)

  // =========================================================================
  // svc 写口
  // =========================================================================
  val wrEn   = sState === S_WR
  val wrBank = Mux(sFound, l.slotBank(sSlot), sBest)
  val wrWay  = Mux(sFound, l.slotWay(sSlot), sFreeW)
  // 新插入：HT 负载 = {指纹, KT 指针}；命中覆盖：负载不变（除 !useKt&&!useAd 的内联 AD）
  val insPay = if (useKt) Cat(sFp, sKtPtr) else if (useAd) Cat(sAdPtr, sKey) else Cat(sAd, sKey)
  val hitPay = if (!useKt && !useAd) Cat(sAd, l.htKey(sPay(sSlot))) else sPay(sSlot)
  val wrPay  = Mux(sFound, hitPay, insPay)
  val htWrEn = wrEn && !sUseOv && (!sFound || (!useKt.B && !useAd.B))
  for (b <- 0 until numBanks) {
    val sel = htWrEn && (wrBank === b.U)
    val wdata = VecInit((0 until ways).map(w => Mux(wrWay === w.U, wrPay, sPay(b * ways + w))))
    htMems(b).io.lgc.we    := sel
    htMems(b).io.lgc.waddr := sIdx(b)
    htMems(b).io.lgc.wdata := wdata.asUInt
  }

  ktMem.foreach { m =>
    val ktWrEn = wrEn && (!sFound || !useAd.B)
    m.io.lgc.we    := ktWrEn
    m.io.lgc.waddr := sKtPtr
    m.io.lgc.wdata := l.ktEntry(sKey, (if (useAd) sAdPtr else sAd))
  }

  adMem.foreach { m =>
    m.io.lgc.we    := wrEn
    m.io.lgc.waddr := sAdPtr
    m.io.lgc.wdata := sAd
  }

  ageTab.io.insEn  := wrEn && !sUseOv
  ageTab.io.insBk  := wrBank
  ageTab.io.insIdx := sIdx(wrBank)
  ageTab.io.insWy  := wrWay

  if (ovfcEn) {
    when(wrEn && sUseOv) {
      ovfcV(sOvSel) := true.B
      ovfcC(sOvSel) := false.B
      ovfcK(sOvSel) := sKey
      ovfcP(sOvSel) := (if (useKt) sKtPtr else if (useAd) sAdPtr else sAd)
      ovfcT(sOvSel) := now
      when(!sFound) { ovfcUseCnt := ovfcUseCnt + 1.U }
    }
  }

  // =========================================================================
  // 归还：删除 / 老化（归还 KT/AD 与清 valid 同拍完成）
  // =========================================================================
  val freeKey = (sState === S_FREE) && sFound
  val agFr    = sState === S_AGFR
  val relEn   = freeKey || agFr

  val ktRelPtr: UInt =
    if (!useKt) 0.U(1.W)
    else Mux(agFr, l.htKtPtr(sPay(agSlot)), sKtPtr)
  val adRelPtr: UInt =
    if (!useAd) 0.U(1.W)
    else Mux(agFr,
      Mux(agOv,
        (if (useKt) l.ktAdPtr(sKtE) else if (ovfcEn) ovfcP(agOvSel) else 0.U(1.W)),
        (if (useKt) l.ktAdPtr(sKtE) else l.htAdPtr(sPay(agSlot)))),
      sAdPtr)

  ktFree.foreach { f => f.io.free := relEn; f.io.faddr := ktRelPtr }
  adFree.foreach { f => f.io.free := relEn; f.io.faddr := adRelPtr }

  ageTab.io.clrEn  := (freeKey && !sUseOv) || (agFr && !agOv)
  ageTab.io.clrBk  := Mux(agFr, agBk, l.slotBank(sSlot))
  ageTab.io.clrIdx := Mux(agFr, agIdx, sIdx(l.slotBank(sSlot)))
  ageTab.io.clrWy  := Mux(agFr, agWy, l.slotWay(sSlot))

  if (ovfcEn) {
    when(freeKey && sUseOv) {
      ovfcV(sOvSel) := false.B; ovfcC(sOvSel) := false.B; ovfcUseCnt := ovfcUseCnt - 1.U
    }
    when(agFr && agOv) {
      ovfcV(agOvSel) := false.B; ovfcC(agOvSel) := false.B; ovfcUseCnt := ovfcUseCnt - 1.U
    }
  }

  when(relEn) {
    when(entryCnt =/= 0.U) { entryCnt := entryCnt - 1.U }
    when(agFr) { cntAgeDrop := cntAgeDrop + 1.U }.otherwise { cntDelete := cntDelete + 1.U }
    sFound := false.B
    sState := S_IDLE
  }
  when(agFr) { agPend := false.B }

  if (learnOn) {
    // 转发 CAM 的弹出要**延迟 2 拍**，不能与 S_WR 同拍：
    //   HT 写在 T 拍落盘（T→T+1 边沿生效），所以在 T-1/T 拍发起 HT 读的请求拿到的是**旧数据**，
    //   它们的比较级分别在 T+1 / T+2 拍；若在 T 拍就弹出 CAM，这些请求会丢掉本应转发来的命中。
    val popPend = RegInit(false.B)
    val popCnt  = RegInit(0.U(3.W))
    when((sState === S_WR) && (sTask === T_LRN)) { popPend := true.B; popCnt := 2.U }
    .elsewhen(popPend) {
      when(popCnt === 0.U) { popPend := false.B }.otherwise { popCnt := popCnt - 1.U }
    }
    fwd.get.io.pop := popPend && (popCnt === 0.U)
  }

  when(sState === S_WR) {
    when(!sFound) { entryCnt := entryCnt + 1.U }
    when(sTask === T_LRN) { cntLearn := cntLearn + 1.U }.otherwise { cntInsert := cntInsert + 1.U }
    sState := S_IDLE
  }

  // =========================================================================
  // 状态输出
  // =========================================================================
  io.status.entries   := entryCnt
  io.status.insert    := cntInsert
  io.status.insFail   := cntInsFail
  io.status.delete    := cntDelete
  io.status.learn     := cntLearn
  io.status.ageDrop   := cntAgeDrop
  io.status.learnDrop := cntLrnDrop
  io.status.fpClash   := cntFpClash
  io.status.ovfcUse   := ovfcUseCnt
  io.status.fwdUse    := (if (learnOn) fwd.get.io.count else 0.U(fwdCntW.W))
  io.status.ktFree    := ktFree.map(f => f.io.count.asUInt).getOrElse(0.U)
  io.status.adFree    := adFree.map(f => f.io.count.asUInt).getOrElse(0.U)
  io.status.lkBusy    := acqV || r1V || d1V
  io.status.mtBusy    := sState =/= S_IDLE
}
