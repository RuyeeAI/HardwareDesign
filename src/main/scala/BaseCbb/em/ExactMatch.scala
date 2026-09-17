package em

import chisel3._
import chisel3.util._
import BaseCbb.memory._

// ===========================================================================
// EM（Exact Match）表模块 —— 流水线版
//
// 对外语义（相对串行版）：
//   io.key   : Valid → Decoupled（多了 ready）。**没有 key FIFO**，靠 ready 反压。
//   io.wr    : Valid → Decoupled（多了 ready），不再静默丢命令。
//   io.rsp   : hit + ad，不变。
//   吞吐     : 命中路径 II=1（CrcHardwired 下）；svc 抢读端口时插入时隙气泡。
//
// ---------------------------------------------------------------------------
// 查找流水线与"影子寄存器"（这是本版最容易写错的地方）
// ---------------------------------------------------------------------------
// 存储读延迟 1 拍：第 T 拍发起读、第 T+1 拍 rdata 有效。
// 所以"发起读的那一级"和"用到数据的那一级"必须错开一拍，metadata 要用影子寄存器跟上：
//
//   拍   T      : S0 接收（哈希组合 + OVFC 比对）
//        T+1    : r1 持有 idx，发起 HT 全 bank 读
//        T+2    : d1 拍 —— HT rdata 有效；d1 携带 {key,idx,ovfc,valid位图}（HT 读的 metadata 影子）
//                 用 HT rdata 组合地形成 KT 读地址并当拍发起
//        T+3    : d2 拍 —— KT rdata 有效；并行比较 + 转发 CAM 比对（miss 判定在这里）
//                 用比较结果组合地形成 AD 读地址并当拍发起
//        T+4    : d3 拍 —— AD rdata 有效 → 输出 rsp
//
//   影子级数随配置裁剪：d1 恒有；useKt||useAd 才有 d2；useKt&&useAd 才有 d3。
//   查找延迟 = 2 + useKt + useAd 拍。
//
// svc 抢端口时 svcOwns=1 → adv=0，**全部流水线寄存器一起冻结**，读地址不变、下一拍重发，
// 数据不会错位（不需要额外的作废逻辑）。
// ---------------------------------------------------------------------------
//
// svc 共享访问引擎（维护口 / 自学习插入 / 老化动作，三者共用）：
//   读地址依赖链导致一次操作要 2 个"时隙"（HT 桶 + KT 候选）；
//   时隙来源 ① 流水线自然空档（零代价）② 等够 slotWaitMax 拍仍没有 →
//   拉低 io.key.ready 反压上游并冻结流水线 1 拍，强行取得时隙。
//   写不需要时隙：HT/KT/AD 都是 TP（同拍 1 读 + 1 写）。
//
// 三条需求的落点：
//   需求1 每拍一请求      → 流水线 + KT 按 (bank,way) 分 bank 并行读，去掉串行候选扫描
//   需求2 背靠背同 KEY     → ForwardCam 在途插入转发（在 miss 判定那一拍压入）
//   需求3 老化并释放 KT    → AgeTable 扫描零带宽 + claim 互斥；归还 KT/AD 与清 valid 同拍
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
  val ovfcUse   = UInt(math.max(1, log2Ceil(l.p.ovfcDepth + 1)).W)
  val fwdUse    = UInt(math.max(1, log2Ceil(l.p.learnFwdDepth + 1)).W)
  // KT / AD 空闲条目总数（直接观测 FreeList，用来验证"老化后资源确实被回收"）
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
  val ktBanks  = l.ktBanks
  val keyW     = l.keyW
  val adW      = l.adW
  val useKt    = params.useKt
  val useAd    = params.useAd
  val ovfcEn   = l.ovfcEn
  val ovfcD    = params.ovfcDepth
  val agingOn  = l.agingOn
  val sweepOn  = l.sweepOn
  val learnOn  = params.learning.isDefined
  val fwdD     = params.learnFwdDepth
  val timeout  = params.aging.map(_.timeout).getOrElse(0)

  private val fwdCntW    = math.max(1, log2Ceil(fwdD + 1))
  private val ovfcCntW1  = math.max(1, log2Ceil(ovfcD + 1))
  private val useD2      = useKt || useAd
  private val useD3      = useKt && useAd

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
  val ktMems: Seq[TpMemoryWrap3] =
    if (useKt) Seq.tabulate(ktBanks) { s =>
      val m = Module(new TpMemoryWrap3(memCfg(s"EmKt$s", l.ktEntryW, l.ktDepthReal)))
      driveAux(m); m
    } else Nil
  val adMem: Option[TpMemoryWrap3] =
    if (useAd) {
      val m = Module(new TpMemoryWrap3(memCfg("EmAd", l.adW, params.adDepth)))
      driveAux(m); Some(m)
    } else None

  private val memReady = htMems.map(_.io.dfx.initDone).reduce(_ && _) &&
    ktMems.map(_.io.dfx.initDone).reduceOption(_ && _).getOrElse(true.B) &&
    adMem.map(_.io.dfx.initDone).getOrElse(true.B) && !io.memInit
  io.memInitDone := memReady

  // =========================================================================
  // CRC / 哈希
  // =========================================================================
  val useSerial = params.crc.isInstanceOf[CrcRuntime]
  val lkCrc = if (useSerial) Some(Module(new CrcSerial(l.crcW, keyW))) else None
  val svCrc = if (useSerial) Some(Module(new CrcSerial(l.crcW, keyW))) else None
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

  val ktFrees: Seq[FreeList] =
    if (useKt) Seq.tabulate(ktBanks)(_ => Module(new FreeList(l.ktDepthReal))) else Nil
  val adFree: Option[FreeList] =
    if (useAd) Some(Module(new FreeList(params.adDepth))) else None
  ktFrees.foreach { f => f.io.alloc := false.B; f.io.free := false.B; f.io.faddr := 0.U }
  adFree.foreach { f => f.io.alloc := false.B; f.io.free := false.B; f.io.faddr := 0.U }

  val fwd = if (learnOn) Some(Module(new ForwardCam(fwdD, keyW, adW))) else None

  // =========================================================================
  // OVFC：HT 溢出 TCAM（寄存器阵列，并行比较）
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
  def ovfcKtSlotOf(i: UInt): UInt = if (ovfcEn && useKt) l.ovfcKtSlot(ovfcP(i)) else 0.U(l.slotW.W)
  def ovfcKtPtrOf(i: UInt): UInt = if (ovfcEn && useKt) l.ovfcKtPtr(ovfcP(i)) else 0.U(l.ktPtrW.W)

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
  val entryCnt   = RegInit(0.U(log2Ceil(params.htDepth * ways + ovfcD + 1).W))

  // =========================================================================
  // svc 引擎：状态与时隙（先声明，流水线推进要用 adv）
  // =========================================================================
  val S_IDLE  = 0.U(4.W); val S_HASH = 1.U(4.W); val S_HTREQ = 2.U(4.W); val S_HTW = 3.U(4.W)
  val S_KTREQ = 4.U(4.W); val S_KTW  = 5.U(4.W); val S_DEC   = 6.U(4.W); val S_ALLOC = 7.U(4.W)
  val S_WR    = 8.U(4.W); val S_FREE = 9.U(4.W); val S_AGFR  = 10.U(4.W)
  val sState = RegInit(S_IDLE)

  // =========================================================================
  // 查找流水线寄存器
  // =========================================================================
  val acqV   = RegInit(false.B)
  val acqKey = Reg(UInt(keyW.W))
  val acqAd  = Reg(UInt(adW.W))
  val acqIdx = Reg(Vec(numBanks, UInt(l.idxW.W)))
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
  val r1OvH = Reg(Bool())
  val r1OvS = Reg(UInt(ovfcCntW1.W))
  val r1OvP = Reg(UInt(math.max(1, l.ovfcPayW).W))

  // ---- d1：HT 数据拍 ----
  val d1V   = RegInit(false.B)
  val d1Key = Reg(UInt(keyW.W))
  val d1Ad  = Reg(UInt(adW.W))
  val d1Idx = Reg(Vec(numBanks, UInt(l.idxW.W)))
  val d1OvH = Reg(Bool())
  val d1OvS = Reg(UInt(ovfcCntW1.W))
  val d1OvP = Reg(UInt(math.max(1, l.ovfcPayW).W))
  val d1Val = Reg(Vec(ktBanks, Bool()))          // HT 各候选的 valid（来自 AgeTable）

  // ---- d2：KT 数据拍（useKt）/ AD 数据拍（!useKt && useAd）----
  val d2V   = RegInit(false.B)
  val d2Key = Reg(UInt(keyW.W))
  val d2Ad  = Reg(UInt(adW.W))
  val d2Idx = Reg(Vec(numBanks, UInt(l.idxW.W)))
  val d2OvH = Reg(Bool())
  val d2OvS = Reg(UInt(ovfcCntW1.W))
  val d2OvP = Reg(UInt(math.max(1, l.ovfcPayW).W))
  val d2Val = Reg(Vec(ktBanks, Bool()))
  val d2Hit = Reg(Bool())                        // !useKt && useAd 时的命中
  val d2FwH = Reg(Bool())
  val d2FwAd = Reg(UInt(adW.W))

  // ---- d3：AD 数据拍（useKt && useAd）----
  val d3V    = RegInit(false.B)
  val d3Hit  = Reg(Bool())
  val d3FwH  = Reg(Bool())
  val d3FwAd = Reg(UInt(adW.W))

  val htRdUsed = r1V
  val ktRdUsed = useKt.B && d1V
  val adRdUsed = useAd.B && (if (useKt) d2V else d1V)
  val pipeFree = !htRdUsed && !ktRdUsed && !adRdUsed

  // 时隙等待计时：svc 需要读端口但流水线在占用 → 等够 slotWaitMax 拍就反压抢一拍
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
  val sPay   = Reg(Vec(ktBanks, UInt(l.htPayW.W)))
  val sVal   = Reg(Vec(ktBanks, Bool()))
  val sClm   = Reg(Vec(ktBanks, Bool()))
  val sKt    = Reg(Vec(ktBanks, UInt(l.ktEntryW.W)))
  val sFound = RegInit(false.B)
  val sUseOv = RegInit(false.B)
  val sOvSel = Reg(UInt(ovfcCntW1.W))
  val sSlot  = Reg(UInt(l.slotW.W))
  val sKtPtr = Reg(UInt(l.ktPtrW.W))
  val sAdPtr = Reg(UInt(l.adPtrW.W))
  val sBest  = Reg(UInt(l.bankW.W))
  val sFreeW = Reg(UInt(l.wayW.W))
  val sKrBase = RegInit(0.U(l.slotW.W))

  // =========================================================================
  // 读地址驱动（流水线与 svc 二选一）
  // =========================================================================
  // HT：r1 拍发起
  for (b <- 0 until numBanks) {
    htMems(b).io.lgc.raddr := Mux(svcOwns, sIdx(b), r1Idx(b))
    htMems(b).io.lgc.re    := Mux(svcOwns, false.B, htRdUsed)
  }
  // AgeTable 桶查询：svc 占时隙时由 svc 驱动（svc 在时隙当拍捕获）
  ageTab.io.qIdx := VecInit((0 until numBanks).map(b => Mux(svcOwns, sIdx(b), r1Idx(b))))

  /** 当拍 HT 回读数据里、槽位 s 对应的 payload（组合） */
  val htRdataV = VecInit(htMems.map(_.io.lgc.rdata))
  def htNow(s: UInt): UInt = {
    val word = htRdataV(l.slotBank(s)).asTypeOf(Vec(ways, UInt(l.htPayW.W)))
    word(l.slotWay(s))
  }

  // KT：d1 拍发起（useKt）
  val svcKtRAddr = VecInit((0 until ktBanks).map(s =>
    Mux(sUseOv || sTask === T_AGE, sKtPtr, (if (useKt) l.htKtPtr(sPay(s)) else 0.U))))
  val ktRdAddrV = VecInit((0 until ktBanks).map { s =>
    if (!useKt) 0.U(l.ktPtrW.W)
    else Mux(d1OvH, l.ovfcKtPtr(d1OvP), l.htKtPtr(htNow(s.U(l.slotW.W))))
  })
  if (useKt) {
    for (s <- 0 until ktBanks) {
      ktMems(s).io.lgc.raddr := Mux(svcOwns, svcKtRAddr(s), ktRdAddrV(s))
      ktMems(s).io.lgc.re    := Mux(svcOwns, false.B, ktRdUsed)
    }
  }

  // =========================================================================
  // 比较级（cm 拍）：useKt 时在 d2，否则在 d1
  // =========================================================================
  val cmV   = if (useKt) d2V else d1V
  val cmKey = if (useKt) d2Key else d1Key
  val cmAd  = if (useKt) d2Ad else d1Ad
  val cmIdx = if (useKt) d2Idx else d1Idx
  val cmVal = if (useKt) d2Val else d1Val
  val cmOvH = if (useKt) d2OvH else d1OvH
  val cmOvS = if (useKt) d2OvS else d1OvS
  val cmOvP = if (useKt) d2OvP else d1OvP

  // 转发 CAM 比对：与 miss 判定同拍 → 背靠背的下一个请求立刻可见
  fwd.foreach(_.io.lookKey := cmKey)
  val fwdHit = if (learnOn) fwd.get.io.hit else false.B
  val fwdAd  = if (learnOn) fwd.get.io.hitAd else 0.U(adW.W)

  // 表内候选比较
  val candHitM = if (useKt) VecInit((0 until ktBanks).map(s => cmVal(s) && l.ktKey(ktMems(s).io.lgc.rdata) === cmKey))
                 else       VecInit((0 until ktBanks).map(s => cmVal(s) && l.htKey(htNow(s.U(l.slotW.W))) === cmKey))
  val candSel = PriorityEncoder(candHitM)
  val tblHit  = Mux(cmOvH, true.B, candHitM.asUInt.orR)
  val selSlot = Mux(cmOvH, ovfcKtSlotOf(cmOvS), candSel)
  val srcEnt  = if (useKt) VecInit(ktMems.map(_.io.lgc.rdata))(selSlot)
                else Mux(cmOvH, cmOvP, htNow(selSlot))
  val tblAdPtr = (if (useAd) (if (useKt) l.ktAdPtr(srcEnt) else l.htAdPtr(srcEnt)) else 0.U(1.W))
  val tblAdVal = (if (useAd) 0.U(1.W) else (if (useKt) l.ktAd(srcEnt) else l.htAd(srcEnt)))
  val hit = tblHit || fwdHit

  // 命中刷新（只写时间戳，不写 valid → 结构上不可能复活已老化条目）
  val rfOn = if (agingOn) cmV && io.ageEn && tblHit else false.B
  val rfBk = l.slotBank(selSlot)
  ageTab.io.rfEn  := rfOn && !cmOvH
  ageTab.io.rfBk  := rfBk
  ageTab.io.rfIdx := cmIdx(rfBk)
  ageTab.io.rfWy  := l.slotWay(selSlot)
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
    r1V := acqV; r1Key := acqKey; r1Ad := acqAd; r1Idx := acqIdx
    r1OvH := acqOvH; r1OvS := acqOvS; r1OvP := acqOvP
    when(acqV) { acqV := false.B }
  }
  when(io.key.valid && io.key.ready) {
    acqKey := io.key.bits
    acqAd  := io.learnAd
    acqOvH := lkOvHitC; acqOvS := lkOvSelC; acqOvP := ovfcPay(lkOvSelC)
    if (useSerial) { lkCrc.get.io.start := true.B; lkCrc.get.io.din := io.key.bits; crcRun := true.B }
    else { acqIdx := slices(hardHash(io.key.bits)); acqV := true.B }
  }
  if (useSerial) {
    when(crcRun && lkCrc.get.io.done) {
      acqIdx := slices(lkCrc.get.io.out); acqV := true.B; crcRun := false.B
    }
  }

  when(adv) {
    // r1 → d1（metadata 影子；valid 位图在此拍从 AgeTable 组合读出）
    d1V := r1V; d1Key := r1Key; d1Ad := r1Ad; d1Idx := r1Idx
    d1OvH := r1OvH; d1OvS := r1OvS; d1OvP := r1OvP
    for (b <- 0 until numBanks) {
      for (w <- 0 until ways) { d1Val(b * ways + w) := ageTab.io.qEnt(b)(w)(0) }
    }
    // d1 → d2
    if (useKt) {
      d2V := d1V; d2Key := d1Key; d2Ad := d1Ad; d2Idx := d1Idx
      d2OvH := d1OvH; d2OvS := d1OvS; d2OvP := d1OvP; d2Val := d1Val
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

  // 启动一次维护/学习任务：命中 OVFC 就直接去读它的 KT，否则先读 HT 桶
  def startTask(k: UInt, a: UInt, op: UInt, task: UInt): Unit = {
    sKey := k; sAd := a; sOp := op; sTask := task
    val ovm = useOvNow(k)
    sUseOv := ovm._1; sOvSel := ovm._2
    if (useSerial) { svCrc.get.io.start := true.B; svCrc.get.io.din := k; sState := S_HASH }
    else {
      when(ovm._1) {
        sSlot := ovfcKtSlotOf(ovm._2); sKtPtr := ovfcKtPtrOf(ovm._2)
        sState := Mux(useKt.B, S_KTREQ, S_DEC)
      }.otherwise {
        sIdx := slices(hardHash(k))
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
        sSlot := ovfcKtSlotOf(agOvSel); sKtPtr := ovfcKtPtrOf(agOvSel)
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
        sIdx := slices(svCrc.get.io.out)
        val ovm = useOvNow(sKey)
        sUseOv := ovm._1; sOvSel := ovm._2
        when(ovm._1) {
          sSlot := ovfcKtSlotOf(ovm._2); sKtPtr := ovfcKtPtrOf(ovm._2)
          sState := Mux(useKt.B, S_KTREQ, S_DEC)
        }.otherwise { sState := S_HTREQ }
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
    when(sTask === T_AGE) {
      sSlot  := l.slotBank(agSlot)
      sKtPtr := (if (useKt) l.htKtPtr(sPay(agSlot)) else 0.U(l.ktPtrW.W))
      sState := Mux(useKt.B, S_KTREQ, S_AGFR)
    }.otherwise {
      sState := Mux(useKt.B, S_KTREQ, S_DEC)
    }
  }

  if (useKt) {
    when(sState === S_KTW) {
      for (s <- 0 until ktBanks) { sKt(s) := ktMems(s).io.lgc.rdata }
      sState := Mux(sTask === T_AGE, S_AGFR, S_DEC)
    }
  }

  // ---- 维护判定 ----
  when(sState === S_DEC) {
    val hitM = VecInit((0 until ktBanks).map { s =>
      sVal(s) && !sClm(s) && (if (useKt) l.ktKey(sKt(s)) === sKey else l.htKey(sPay(s)) === sKey)
    })
    val sel = PriorityEncoder(hitM)
    val adpOf: UInt => UInt = (sl: UInt) => {
      if (!useAd) 0.U(1.W)
      else if (useKt) l.ktAdPtr(sKt(sl))
      else l.htAdPtr(sPay(sl))
    }
    when(sUseOv) {
      sFound := true.B
      if (useAd) {
        val a = if (useKt) l.ktAdPtr(sKt(sSlot))
                else if (ovfcEn) l.htAdPtr(ovfcP(sOvSel))
                else 0.U(1.W)
        sAdPtr := a
      }
      sState := Mux(sOp === EmOp.del, S_FREE, S_WR)
    }.elsewhen(hitM.asUInt.orR) {
      sFound := true.B
      sSlot  := sel
      sKtPtr := (if (useKt) l.htKtPtr(sPay(sel)) else 0.U(l.ktPtrW.W))
      if (useAd) { sAdPtr := adpOf(sel) }
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
  val allocSlotW = Wire(UInt(l.slotW.W))
  val allocWill  = Wire(Bool())
  allocWill := false.B
  allocSlotW := 0.U
  when(sState === S_ALLOC) {
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
    val htFree   = freeMask.asUInt.orR
    val htSlot   = bestBank * ways.U + PriorityEncoder(freeMask)

    val ovOrder = VecInit((0 until ktBanks).map { k =>
      val s = sKrBase +& k.U(l.slotW.W)
      Mux(s >= ktBanks.U, s - ktBanks.U, s)(l.slotW - 1, 0)
    })
    val ovOk   = if (useKt) VecInit(ovOrder.map(s => VecInit(ktFrees.map(_.io.ok))(s))) else VecInit(Seq(false.B))
    val ovSlot = ovOrder(PriorityEncoder(ovOk))

    sUseOv := !htFree
    sBest  := bestBank
    sFreeW := PriorityEncoder(freeMask)
    sSlot  := Mux(htFree, htSlot, ovSlot)
    if (ovfcEn) { when(!htFree) { sOvSel := ovfcFreeSel } }

    allocWill  := htFree || ovfcHasFree
    allocSlotW := Mux(htFree, htSlot, ovSlot)

    val ktOk = (if (useKt) VecInit(ktFrees.map(_.io.ok))(allocSlotW) else true.B)
    val adOk = adFree.map(_.io.ok).getOrElse(true.B)
    when(allocWill && ktOk && adOk) {
      sKtPtr := (if (useKt) VecInit(ktFrees.map(_.io.addr))(allocSlotW) else 0.U(l.ktPtrW.W))
      adFree.foreach(f => sAdPtr := f.io.addr)
      sState := S_WR
    }.otherwise {
      sState := S_IDLE
      cntInsFail := cntInsFail + 1.U
    }
  }
  if (useKt) {
    for (s <- 0 until ktBanks) { ktFrees(s).io.alloc := allocWill && (allocSlotW === s.U) }
  }
  adFree.foreach(_.io.alloc := allocWill)

  // =========================================================================
  // svc 写口
  // =========================================================================
  val wrEn   = sState === S_WR
  val wrBank = Mux(sFound, l.slotBank(sSlot), sBest)
  val wrWay  = Mux(sFound, l.slotWay(sSlot), sFreeW)
  val insPay = if (useKt) sKtPtr else if (useAd) Cat(sAdPtr, sKey) else Cat(sAd, sKey)
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

  if (useKt) {
    val ktWrEn = wrEn && (!sFound || !useAd.B)
    val ktEnt  = l.ktEntry(sKey, (if (useAd) sAdPtr else sAd))
    for (s <- 0 until ktBanks) {
      ktMems(s).io.lgc.we    := ktWrEn && (sSlot === s.U)
      ktMems(s).io.lgc.waddr := sKtPtr
      ktMems(s).io.lgc.wdata := ktEnt
    }
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
      ovfcP(sOvSel) := (if (useKt) Cat(sSlot, sKtPtr) else if (useAd) sAdPtr else sAd)
      ovfcT(sOvSel) := now
      when(!sFound) {
        ovfcUseCnt := ovfcUseCnt + 1.U
        sKrBase := Mux(sKrBase === (ktBanks - 1).U, 0.U, sKrBase + 1.U)
      }
    }
  }

  // =========================================================================
  // 归还：删除 / 老化（归还 KT/AD 与清 valid 同拍完成）
  // =========================================================================
  val freeKey = (sState === S_FREE) && sFound
  val agFr    = sState === S_AGFR
  val relEn   = freeKey || agFr

  val ktRelSlot = Mux(agFr, Mux(agOv, sSlot, l.slotBank(agSlot)), sSlot)
  val ktRelPtr  = Mux(agFr,
    Mux(agOv, sKtPtr, (if (useKt) l.htKtPtr(sPay(agSlot)) else 0.U(l.ktPtrW.W))),
    sKtPtr)
  val adRelPtr: UInt =
    if (!useAd) 0.U(1.W)
    else Mux(agFr,
      Mux(agOv,
        (if (useKt) l.ktAdPtr(sKt(sSlot)) else if (ovfcEn) ovfcP(agOvSel) else 0.U(1.W)),
        (if (useKt) l.ktAdPtr(sKt(agSlot)) else l.htAdPtr(sPay(agSlot)))),
      sAdPtr)

  if (useKt) {
    for (s <- 0 until ktBanks) {
      ktFrees(s).io.free  := relEn && (ktRelSlot === s.U)
      ktFrees(s).io.faddr := ktRelPtr
    }
  }
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
    //   延迟 2 拍后（CAM 从 T+3 起为空）覆盖全部在途请求；而 svc 完成两次插入至少间隔 7 拍，
    //   因此单个 pending 计数器足够，不会重叠。
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
  // 临时调试：把比较级信息随响应打一拍，便于 testbench 观测
  // =========================================================================
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
  io.status.ovfcUse   := ovfcUseCnt
  io.status.fwdUse    := (if (learnOn) fwd.get.io.count else 0.U(fwdCntW.W))
  io.status.ktFree    := (if (useKt) ktFrees.map(f => f.io.count.asUInt).reduce(_ +& _) else 0.U)
  io.status.adFree    := adFree.map(f => f.io.count.asUInt).getOrElse(0.U)
  io.status.lkBusy    := acqV || r1V || d1V
  io.status.mtBusy    := sState =/= S_IDLE
}
