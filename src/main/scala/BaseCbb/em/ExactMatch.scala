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

/** UE（存储读出不可纠错误）来源编码，对应 `EmIo.memUErrSrc` */
object EmUErrSrc {
  val ht = 0.U(2.W)   // HT 桶字（指纹/KT 指针）不可纠
  val kt = 1.U(2.W)   // KT Full Key 不可纠
  val ad = 2.U(2.W)   // AD 动作数据不可纠
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
  val uerrCnt   = UInt(16.W)   // 存储读出不可纠错误（UE）次数：查找 + 维护合计
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
  // ---- 存储不可纠错误（UE）上报 ----
  // memUErr 单拍脉冲；memUErrSrc 给出是 HT/KT/AD 哪张表（见 EmUErrSrc）。
  // 该脉冲与"这一拍上报的响应"同拍：遇到 UE 的请求会被**强制判 miss**（ad 一并清零），
  // 所以消费方不需要额外的 error 位 —— 看到脉冲就知道刚刚那个 miss 是"因为存储坏了"。
  val memUErr    = Output(Bool())
  val memUErrSrc = Output(UInt(2.W))
  // ---- 存储 UE 注入（DFX / RAS 验证用，正常工作时恒 0）----
  // 语义同 MemoryDfxPort.injUerrEn：在"发起读"那一拍拉高，就让该次读报不可纠错误。
  // injUerrSrc 选中注入到哪张表（见 EmUErrSrc）；只有注入源匹配的表会报错。
  val injUerrEn  = Input(Bool())
  val injUerrSrc = Input(UInt(2.W))
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
    // 四个插拍由 EmParams 参数化（默认全 false → 读延时 1 拍）。
    // 打开后读延时变长，流水线的影子寄存器会自动跟着延（见下面的 shadow）。
    flopIn     = params.memFlopIn,
    flopOut    = params.memFlopOut,
    CheckIn    = params.memCheckIn,
    CheckOut   = params.memCheckOut,
    RsAccess   = false,
    initValue  = MemoryInitType.AllZero
  )

  /** 只接维护/初始化/注入这些"旁路"信号；读写数据通路在下面单独接。 */
  private def driveAux(m: TpMemoryWrap3, injSel: UInt): Unit = {
    m.io.cpu.we    := false.B
    m.io.cpu.re    := false.B
    m.io.cpu.addr  := 0.U
    m.io.cpu.wdata := 0.U
    m.io.cpuCfg.idleCycleTh0 := 0.U
    m.io.dfx.init  := io.memInit
    m.io.dfx.injCorrEn := false.B
    // UE 注入：injUerrSrc 选中哪张表就注到哪张（见 EmUErrSrc）
    m.io.dfx.injUerrEn := io.injUerrEn && (io.injUerrSrc === injSel)
  }

  val htMems = Seq.tabulate(numBanks) { b =>
    // 宽度必须是**整桶字** htWordW = htPayW*ways：HT 一次读一整桶，ways 条 payload 拼成一个字
  val m = Module(new TpMemoryWrap3(memCfg(s"EMHT_INST$b", l.htWordW, l.bankDepth)))
    driveAux(m, EmUErrSrc.ht); m
  }
  // KT 单实例（每次查找只读 1 路）
  val ktMem: Option[TpMemoryWrap3] =
    if (useKt) {
    val m = Module(new TpMemoryWrap3(memCfg("EMKT", l.ktEntryW, l.ktDepthReal)))
      driveAux(m, EmUErrSrc.kt); Some(m)
    } else None
  val adMem: Option[TpMemoryWrap3] =
    if (useAd) {
      val m = Module(new TpMemoryWrap3(memCfg("EMAD", l.adW, params.adDepth)))
      driveAux(m, EmUErrSrc.ad); Some(m)
    } else None

  /**
   * 老化时间戳表（逐条 ts，只给后台扫描器用）。
   *
   * 独立 SRAM 而不是寄存器阵列：逐条 ts 用寄存器是 ageWords×ageW 个 flop，比 HT SRAM 还大。
   * 配置固定（无 flop、无保护、rdLat=1）——它不在查找关键路径上，也不需要 ECC
   * （ts 位翻转只会让某条晚删/早删，不影响数据面正确性），所以不跟随
   * `memFlopIn/Out` 和 `memProtect`（后者只覆盖 HT/KT/AD 三张数据表）。
   * AgeTable 的扫描器按 rdLat=1 对齐（延迟一拍游标），改这里必须同步改 AgeTable。
   */
  private def ageMemCfg: Memory = Memory(
    name       = "EMAGETS",
    dataType   = UInt(math.max(1, l.ageW).W),
    depth      = l.ageTsDepth,
    memoryType = MemoryAccessType.TP,
    protect    = MemoryProtectType.ProtNone,
    flopIn = false, flopOut = false, CheckIn = false, CheckOut = false,
    RsAccess   = false,
    initValue  = MemoryInitType.AllZero
  )
  // 老化信息表（valid/claim 寄存器阵列 + ts SRAM 的读写口）。声明放在这里是因为
  // 它下面的 ageMem 要接它的 ts 端口；其余 io 在各自用到的地方驱动。
  val ageTab = Module(new AgeTable(l, timeout))

  val ageMem: Option[TpMemoryWrap3] =
    if (agingOn) { require(ageMemCfg.readLatency == 1, "老化 ts SRAM 必须是 rdLat=1（AgeTable 按此对齐）"); Some(Module(new TpMemoryWrap3(ageMemCfg))) }
    else None
  ageMem.foreach { m =>
    // ts 不参与 UE 上报/注入：injUerrEn 恒 0
    m.io.cpu.we := false.B; m.io.cpu.re := false.B; m.io.cpu.addr := 0.U; m.io.cpu.wdata := 0.U
    m.io.cpuCfg.idleCycleTh0 := 0.U
    m.io.dfx.init := io.memInit
    m.io.dfx.injCorrEn := false.B
    m.io.dfx.injUerrEn := false.B
    m.io.lgc.raddr := ageTab.io.tsRaddr
    m.io.lgc.re    := ageTab.io.tsRe
    m.io.lgc.we    := ageTab.io.tsWe
    m.io.lgc.waddr := ageTab.io.tsWaddr
    m.io.lgc.wdata := ageTab.io.tsWdata
  }
  // ts 回读（rdLat=1：扫描器发起读的下一拍有效）；未启用老化时没有这块 SRAM
  ageTab.io.tsRd := ageMem.map(_.io.lgc.rdata).getOrElse(0.U)

  // 读延时由四个插拍参数决定（见 EmParams.memFlopIn/Out/CheckIn/Out）。流水线各级之间按
  // `l.rdLat` 延拍：这里的 require 是把 EmLayout 的推导与 Memory 的实现**互相校验**，
  // 防止两边公式漂移（也顺便保证三张表一致 —— 它们共用同一组插拍参数）。
  // ts SRAM 用的是自己的固定配置（rdLat=1），不参与这里的校验，见 ageMemCfg。
  private def latOf(w: Int, d: Int): Int = memCfg("latProbe", w, d).readLatency
  require(latOf(l.htWordW, l.bankDepth) == l.rdLat,
    s"HT 读延时：Memory 算出 ${latOf(l.htWordW, l.bankDepth)}，EmLayout 算出 ${l.rdLat}")
  require(latOf(l.ktEntryW, l.ktDepthReal) == l.rdLat,
    s"KT 读延时：Memory 算出 ${latOf(l.ktEntryW, l.ktDepthReal)}，EmLayout 算出 ${l.rdLat}")
  require(latOf(l.adW, params.adDepth) == l.rdLat,
    s"AD 读延时：Memory 算出 ${latOf(l.adW, params.adDepth)}，EmLayout 算出 ${l.rdLat}")

  private val memReady = htMems.map(_.io.dfx.initDone).reduce(_ && _) &&
    ktMem.map(_.io.dfx.initDone).getOrElse(true.B) &&
    adMem.map(_.io.dfx.initDone).getOrElse(true.B) &&
    ageMem.map(_.io.dfx.initDone).getOrElse(true.B) && !io.memInit
  io.memInitDone := RegNext(memReady,false.B)

  // =========================================================================
  // 存储不可纠错误（UE）
  //
  // Wrap3 的 uecErr 与 rdata **同拍**（Memory.scala 里 errOutReg/rdataOutReg 对齐，已实测），
  // 所以按"发起读的下一拍"采样即可：HT 在 d1 采、KT 在 d2 采、AD 在响应拍采。
  //
  // 策略（fail-safe —— 宁可报 miss，也绝不用坏数据）：
  //   ① 命中判定不可信 → 强制 miss，响应里 ad 一并清零（坏数据不外传）；
  //   ② 计数 + 顶层脉冲/来源上报（本请求的响应那一拍）；
  //   ③ UE 自愈：把坏掉的 HT 槽位作废（见下面 invPend），交给 svc 空闲时执行；
  //   ④ svc（维护/老化）读到 UE → 放弃本次任务，绝不拿坏数据写回（见 S_MEMERR）。
  //
  // ⚠️ `memProtect = ProtNone`（EM 默认）时 uecErr 恒 0，这一整套是纯旁路、不改变行为。
  // ⚠️ KT 读出错若不拦，`ktKey === cmKey` 可能**假命中**返回别人的 ad —— 静默的数据面错误，
  //    所以 KT 的 UE 是这里最要紧的一条。
  // =========================================================================
  private val htUErrV = VecInit(htMems.map(_.io.lgc.uecErr))
  private val htUErrC = htUErrV.asUInt.orR
  private val ktUErrC = ktMem.map(_.io.lgc.uecErr).getOrElse(false.B)
  private val adUErrC = adMem.map(_.io.lgc.uecErr).getOrElse(false.B)

  val cntUErr = RegInit(0.U(16.W))

  // ---- UE 自愈：待作废的 HT 槽位（bank/idx + way 位掩码）----
  // 为什么用掩码：HT 读出错时**只知道桶**（整个桶字不可信，无法判断是哪一路），
  // 要作废该桶全部 way；KT/AD 读出错时明确知道是命中那一路，只作废 1 位。
  // 清 valid 而不归还 KT/AD：指针来自坏数据不可信，宁可有界泄漏也不乱释放。
  val invPend = RegInit(false.B)
  val invBk   = Reg(UInt(l.bankW.W))
  val invIdx  = Reg(UInt(l.idxW.W))
  val invMask = Reg(UInt(ways.W))
  val invBusy = invPend && invMask =/= 0.U

  /** 请求作废某桶的若干路。⚠️ 同一拍只接受一个请求（并行 UE 属致命事件，丢一个可接受）。 */
  private def invReq(bk: UInt, idx: UInt, mask: UInt): Unit = {
    invPend := true.B
    invBk   := bk
    invIdx  := idx
    invMask := mask
  }
  private def invAllWays: UInt = ((BigInt(1) << ways) - 1).U(ways.W)
  private def invOneWay(w: UInt): UInt = (1.U(ways.W) << w)(ways - 1, 0)
  // 作废掩码当前选中的路（顶层推进掩码与 SvcEngine 生成 clr 地址都要用）
  val invWySel  = PriorityEncoder(invMask)
  val invOneHot = UIntToOH(invWySel, ways)

  // =========================================================================
  // CRC / 哈希
  // =========================================================================
  val useSerial = params.crc.isInstanceOf[CrcRuntime]
  // 查找侧的串行 CRC；维护侧的串行 CRC 在 SvcEngine 里（见那里）
  val lkCrc = if (useSerial) Some(Module(new CrcSerial(l.crcW, keyW, params.crc.refin, params.crc.refout))) else None
  lkCrc.foreach { m =>
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
  // 空闲栈 / 转发 CAM
  // =========================================================================
  val ktFree: Option[FreeList] = if (useKt) Some(Module(new FreeList(l.ktDepthReal))) else None
  val adFree: Option[FreeList] = if (useAd) Some(Module(new FreeList(params.adDepth))) else None
  ktFree.foreach { f => f.io.alloc := false.B; f.io.free := false.B; f.io.faddr := 0.U }
  adFree.foreach { f => f.io.alloc := false.B; f.io.free := false.B; f.io.faddr := 0.U }

  val fwd = if (learnOn) Some(Module(new ForwardCam(fwdD, keyW, adW))) else None

  // =========================================================================
  // OVFC：HT 溢出表（自包含于 OvfTable；这里只做端口连接）
  // =========================================================================
  val ovf = Module(new OvfTable(l, timeout))
  ovf.io.now := now
  ovf.io.lkKey := io.key.bits
  // 其余端口在各自用到的地方驱动（svc 的匹配/扫描/写口等）

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

  val lkOvHitC = ovf.io.lkHit
  val lkOvSelC = ovf.io.lkSel

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

  // ---- UE 影子 ----
  // ⚠️ 采样拍子：Wrap3 的 uecErr 与 rdata **同拍**，而三张表的 rdata 分别在
  //    HT→d1、KT→d2、AD→d3（或 !useKt 时 d2）才有效，所以 uecErr 也只在那些拍有效。
  //    HT 的错在 d1 拍是**组合有效**（与 htNow 同源）—— 不能像 metadata 那样"在 r1 拍捕获"，
  //    那会采到上一拍的 0（这是 v1 踩过的坑）。要带过 d2/d3 才需要寄存器。
  //    KT/AD 的错本身就在各自数据拍有效，直接用当时的组合值。
  val shHtUe  = RegInit(false.B)     // 随 d1→d2 推进：本请求的 HT 读错
  val d3UeAcc = RegInit(false.B)     // 随 d2→d3 推进：d2 拍为止的累计
  val d3UeSrc = Reg(UInt(2.W))
  // 发起 AD 读时选中的 HT 槽位（随流水带到 AD 数据拍，AD 读报 UE 时用它定位要作废的槽位）
  val adSBk   = Reg(UInt(l.bankW.W))
  val adSWy   = Reg(UInt(l.wayW.W))
  val adSIdx  = Reg(UInt(l.idxW.W))
  // ⚠️ 该槽位是否真的"来自 HT 槽位匹配"。OVFC 命中时 adSBk/adSWy 无意义（指针来自 ovfcP），
  //    miss 时更无意义（PriorityEncoder 全 0 输出）—— AD 读报 UE 时若拿它去作废，会误删
  //    一个毫不相干的条目。只有 HT 槽位匹配命中时才允许作废。
  val adSValid = if (useAd) Reg(Bool()) else false.B

  val htRdUsed = r1V
  val ktRdUsed = useKt.B && d1V
  val adRdUsed = useAd.B && (if (useKt) d2V else d1V)
  val pipeFree = !htRdUsed && !ktRdUsed && !adRdUsed

  // =========================================================================
  // 老化：扫描 → claim → 一个 pending 目标（同时只处理一个）
  // 仲裁/claim 胶水收在 AgeSched 里（见那里对"claim 与记录目标必须同拍同优先级"的说明）
  // =========================================================================
  val scanOn  = (if (agingOn && sweepOn) io.ageEn && memReady else false.B)

  ageTab.io.now    := now
  ageTab.io.scanEn := scanOn

  // OVFC 侧扫描（游标在 OvfTable 内部）
  ovf.io.scanEn    := scanOn
  ovf.io.scanBlock := invBusy

  val agSched = Module(new AgeSched(l))
  agSched.io.scanEn  := scanOn
  agSched.io.invBusy := invBusy
  agSched.io.htHit := ageTab.io.scanHit
  agSched.io.htBk  := ageTab.io.scanBk
  agSched.io.htIdx := ageTab.io.scanIdx
  agSched.io.htWy  := ageTab.io.scanWy
  agSched.io.ovHit := ovf.io.scanHit
  agSched.io.ovSel := ovf.io.scanSel

  ageTab.io.scanHold := agSched.io.pend
  ageTab.io.clmEn    := agSched.io.htClmEn
  ovf.io.scanHold    := agSched.io.pend
  ovf.io.clmEn       := agSched.io.ovClmEn
  ovf.io.clmSel      := ovf.io.scanSel

  // pending 目标的只读别名（寄存器实体在 AgeSched 里；这里只读，写一律走 release）
  val agPend  = agSched.io.pend
  val agOv    = agSched.io.pendOv
  val agOvSel = agSched.io.pendOvSel
  ovf.io.agSel := agOvSel

  // 转发 CAM 是否还有待学的 head（自学习任务的来源）
  val fwdPending = if (learnOn) !fwd.get.io.empty else false.B

  // =========================================================================
  // svc 共享访问引擎（阶段3 抽出的子模块）
  //
  // 引擎拥有"维护 / 自学习插入 / 老化动作"的状态机与决策；**存储实例与读写地址 mux 留在
  // 这里**：引擎给出"svc 侧"的读写请求，下面用 svcOwns 与查找流水线二选一。
  // AgeTable / OVFC / FreeList / AgeSched 的写口与释放口由引擎直接驱动。
  // 计数与 entryCnt 留在顶层，引擎只给单拍脉冲（insDone/insFail/...）。
  // =========================================================================
  val eng = Module(new SvcEngine(l, params))
  eng.io.pipeFree := pipeFree
  eng.io.memReady := memReady
  eng.io.wrValid  := io.wr.valid
  eng.io.wrOp     := io.wr.bits.op
  eng.io.wrKey    := io.wr.bits.key
  eng.io.wrAd     := io.wr.bits.ad
  eng.io.fwdPend  := fwdPending
  eng.io.fwdKey   := (if (learnOn) fwd.get.io.headKey else 0.U)
  eng.io.fwdAd    := (if (learnOn) fwd.get.io.headAd else 0.U)
  eng.io.agPend   := agSched.io.pend
  eng.io.agOv     := agSched.io.pendOv
  eng.io.agOvSel  := agSched.io.pendOvSel
  eng.io.agSlot   := agSched.io.pendSlot
  eng.io.agBk     := agSched.io.pendBk
  eng.io.agIdx    := agSched.io.pendIdx
  eng.io.agWy     := agSched.io.pendWy
  eng.io.agClaim  := ovf.io.agClaim
  eng.io.agPay    := ovf.io.agPay
  eng.io.ovSvHit   := ovf.io.svHit
  eng.io.ovSvSel   := ovf.io.svSel
  eng.io.ovSvPay   := ovf.io.svPay
  eng.io.ovSvPayQ  := ovf.io.svPayQ
  eng.io.ovHasFree := ovf.io.hasFree
  eng.io.ovAlloc   := ovf.io.allocSel
  eng.io.htRdata := VecInit(htMems.map(_.io.lgc.rdata))
  eng.io.ktRdata := ktMem.map(_.io.lgc.rdata).getOrElse(0.U)
  eng.io.htQEnt  := ageTab.io.qEnt
  eng.io.htUErr  := htUErrC
  eng.io.ktUErr  := ktUErrC
  eng.io.ktOk    := ktFree.map(_.io.ok).getOrElse(true.B)
  eng.io.ktAddr  := ktFree.map(_.io.addr).getOrElse(0.U)
  eng.io.adOk    := adFree.map(_.io.ok).getOrElse(true.B)
  eng.io.adAddr  := adFree.map(_.io.addr).getOrElse(0.U)
  eng.io.invBusy := invBusy
  eng.io.invBk   := invBk
  eng.io.invIdx  := invIdx
  eng.io.invWy   := invWySel
  eng.io.clrWasValid := ageTab.io.clrWasValid
  eng.io.crcPoly := io.crcPoly
  eng.io.crcInit := io.crcInit
  eng.io.crcXor  := io.crcXor

  /** svc 抢占读端口时冻结查找流水线（引擎内部按时隙仲裁给出） */
  val svcOwns = eng.io.svcOwns
  val adv     = !svcOwns

  // ---- svc 的写口直连（只有 svc 会写存储；流水线只读）----
  for (b <- 0 until numBanks) {
    htMems(b).io.lgc.we    := eng.io.htWe(b)
    htMems(b).io.lgc.waddr := eng.io.htWaddr(b)
    htMems(b).io.lgc.wdata := eng.io.htWdata(b)
  }
  ktMem.foreach { m =>
    m.io.lgc.we    := eng.io.ktWe
    m.io.lgc.waddr := eng.io.ktWaddr
    m.io.lgc.wdata := eng.io.ktWdata
  }
  adMem.foreach { m =>
    m.io.lgc.we    := eng.io.adWe
    m.io.lgc.waddr := eng.io.adWaddr
    m.io.lgc.wdata := eng.io.adWdata
  }

  // ---- AgeTable 写口：ins/clr 由引擎驱动；rf（命中刷新）仍由流水线驱动，见下 ----
  ageTab.io.insEn  := eng.io.agInsEn
  ageTab.io.insBk  := eng.io.agInsBk
  ageTab.io.insIdx := eng.io.agInsIdx
  ageTab.io.insWy  := eng.io.agInsWy
  ageTab.io.clrEn  := eng.io.agClrEn
  ageTab.io.clrBk  := eng.io.agClrBk
  ageTab.io.clrIdx := eng.io.agClrIdx
  ageTab.io.clrWy  := eng.io.agClrWy

  // ---- OVFC 写 / 释放 / 按序号取 payload ----
  ovf.io.svKey     := eng.io.ovMatchKey
  ovf.io.wrEn      := eng.io.ovWrEn
  ovf.io.wrSel     := eng.io.ovWrSel
  ovf.io.wrKey     := eng.io.ovWrKey
  ovf.io.wrPay     := eng.io.ovWrPay
  ovf.io.wrCountUp := eng.io.ovWrCntUp
  ovf.io.freeEn    := eng.io.ovFreeEn
  ovf.io.freeSel   := eng.io.ovFreeSel
  ovf.io.svPaySel  := eng.io.ovSvPaySel

  // ---- FreeList 分配 / 释放 ----
  ktFree.foreach { f => f.io.alloc := eng.io.ktAlloc; f.io.free := eng.io.ktFreeEn; f.io.faddr := eng.io.ktFreeAddr }
  adFree.foreach { f => f.io.alloc := eng.io.adAlloc; f.io.free := eng.io.adFreeEn; f.io.faddr := eng.io.adFreeAddr }

  // ---- 老化 pending 目标的释放（放掉后扫描器才能 claim 下一个）----
  agSched.io.release := eng.io.agRelease

  io.wr.ready := eng.io.wrReady

  // ---- 统计计数（实体留在这里，引擎只给脉冲）----
  when(eng.io.insDone)   { cntInsert  := cntInsert  + 1.U }
  when(eng.io.insFail)   { cntInsFail := cntInsFail + 1.U }
  when(eng.io.delDone)   { cntDelete  := cntDelete  + 1.U }
  when(eng.io.learnDone) { cntLearn   := cntLearn   + 1.U }
  when(eng.io.ageDrop)   { cntAgeDrop := cntAgeDrop + 1.U }
  when(eng.io.fpClash)   { cntFpClash := cntFpClash + 1.U }
  when(eng.io.entryInc)  { entryCnt   := entryCnt   + 1.U }
  // UE 自愈作废、S_MEMERR 放弃老化时的作废，条目数都要同步减（引擎给脉冲）
  when(eng.io.entryDec)  { when(entryCnt =/= 0.U) { entryCnt := entryCnt - 1.U } }

  // UE 自愈：作废请求由引擎在 S_IDLE 里发起，掩码推进留在这里（每拍清一路）
  when(eng.io.invGo) {
    invMask := invMask & ~invOneHot
    when((invMask & ~invOneHot) === 0.U) { invPend := false.B }
  }

  // =========================================================================
  // 读地址驱动（流水线与 svc 二选一）
  // =========================================================================
  // HT：r1 拍发起（每 bank 独立一次）；svc 抢到 S_HTREQ 那一拍由 svc 发起
  // ⚠️ svc 也必须拉高 re：v1 曾写死 `Mux(svcOwns, false.B, htRdUsed)`，仿真里因为
  // SimMemory 的 rdata 恒等于 RegNext(m(raddr))、不按 re 门控而"看起来能用"，
  // 但 (a) 换成物理 SRAM 后 svc 的 HT 读会拿不到数据，(b) uecErr 被 reFlopped 门掉，
  // svc 的 UE 永远不会上报。KT/AD 那两处本来就是"发起读那一拍拉 re"的写法。
  val htRaddrMux = VecInit((0 until numBanks).map(b => Mux(svcOwns, eng.io.htRaddr(b), r1Idx(b))))
  for (b <- 0 until numBanks) {
    htMems(b).io.lgc.raddr := htRaddrMux(b)
    htMems(b).io.lgc.re    := Mux(svcOwns, eng.io.htRe, htRdUsed)
  }
  // AgeTable 的桶查询与 HT 读地址完全同源（同一组 idx），直接复用这棵 mux
  ageTab.io.qIdx := htRaddrMux

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

  // svc 的 KT 读地址（维护 → 指纹命中槽位；老化 → 被 claim 的槽位）由引擎给出
  ktMem.foreach { m =>
    m.io.lgc.raddr := Mux(svcOwns, eng.io.ktRaddr, ktRdAddrPipe)
    m.io.lgc.re    := Mux(svcOwns, eng.io.ktRe, ktRdUsed)
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

  // ---- UE 各拍的组合值（采样拍子见上面影子寄存器的说明）----
  // d1 拍：HT 桶字读的错（HT 数据就在 d1 拍）
  val d1UeC    = d1V && htUErrC
  // d2 拍：d1 带过来的 HT 错 + 本拍 KT（useKt）或 AD（!useKt&&useAd）数据的错
  val ktUeC    = if (useKt) (d2V && ktUErrC) else false.B
  val adUeC    = if (useD3) false.B else if (useAd) (d2V && adUErrC) else false.B
  val d2UeC    = shHtUe || ktUeC || adUeC
  val d2UeSrcC = Mux(shHtUe, EmUErrSrc.ht, Mux(ktUeC, EmUErrSrc.kt, EmUErrSrc.ad))
  // d3 拍：d2 带过来的累计 + 本拍 AD 数据的错
  val d3AdC    = if (useD3) (d3V && adUErrC) else false.B
  val d3UeSrcC = Mux(d3UeAcc, d3UeSrc, EmUErrSrc.ad)

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
  // UE fail-safe：决策拍已知该请求读到过不可纠错误 → 表内命中判定不可信，强制 miss。
  // 这一处门控同时管住 hit / 命中刷新(rfOn) / 自学习判定(learnReq) / d2Hit / d3Hit，
  // 也就是所有"是否命中"的下游用法；AD 读的错在响应拍才到，另在响应处门控。
  // （learning 打开时，强制 miss 会把该 key 重新压进转发 CAM → 走一次 add 覆盖写，
  //   等于顺带把坏条目修好 —— 自愈的[快路径]。）
  val cmUe  = if (useKt) d2UeC else d1UeC
  val tblHit = Mux(cmOvH, true.B, cmHit) && !cmUe

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
  // OVFC 命中项的刷新：只更新它的 ts（HT 侧的刷新走 ageTab.io.rfEn）
  ovf.io.rfEn  := rfOn && cmOvH
  ovf.io.rfSel := cmOvS

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
  // ⚠️ 只有"表内命中"才真的需要 AD 数据。miss / 纯转发命中时 ad 用不上，而地址来自
  //    `tblAdPtr`（useKt 且未命中指纹时是 KT[0] 里的垃圾指针）——既白占一次 AD 带宽，
  //    又可能在一个无关地址上撞出 UE，进而触发下面的"AD UE 自愈"去作废不相干的槽位。
  // =========================================================================
  private val adRdNeed = adRdUsed && tblHit && !fwdHit
  adMem.foreach { m =>
    m.io.lgc.raddr := Mux(fwdHit, 0.U, tblAdPtr)
    m.io.lgc.re    := Mux(svcOwns, false.B, adRdNeed)
  }

  // =========================================================================
  // 流水线推进（svcOwns 时整体冻结）
  // =========================================================================
  /**
   * 把上一级的影子值搬到本级。读延时是 rdLat 拍，所以"发起读的那一级"到
   * "用数据的那一级"要隔 rdLat 个寄存器（rdLat==1 时就是原来的 `RegNext`）。
   *
   * 为什么用 `RegEnable(_, adv)` 而不是 `ShiftRegister`：
   *   ① 不带复位 —— 流水线 reg 原本就是 `Reg`，加复位会给已经很重的复位网络再添负载；
   *   ② 链上每一级都跟着 `adv` 冻结，所以在 `when(adv)` 内外写都对。
   * 注意 `acq → r1` 那一步与存储无关，仍然是 1 拍，不走这里。
   */
  private def shadow[T <: Data](x: T): T =
    (1 until l.rdLat).foldLeft(x)((prev, _) => RegEnable(prev, adv))

  // ⚠️ 必须带上 memReady：存储初始化（或重新初始化）期间 HT/KT/AD 里是未定义值，
  //    放请求进来会读到垃圾并可能产生假命中。svc 侧本来就用 memReady 门控，查找侧对齐。
  io.key.ready := adv && memReady && (if (useSerial) !crcRun else true.B)

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
    acqOvH := lkOvHitC; acqOvS := lkOvSelC; acqOvP := ovf.io.lkPay
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
    // r1 → d1：metadata 影子（按 rdLat 延拍）+ 指纹比对结果
    //（AgeTable 的 valid 位图在 r1 拍组合读出，所以也要跟着延相同拍数）
    d1V := shadow(r1V); d1Key := shadow(r1Key); d1Ad := shadow(r1Ad)
    d1Fp := shadow(r1Fp)
    d1OvH := shadow(r1OvH); d1OvS := shadow(r1OvS); d1OvP := shadow(r1OvP)
    for (b <- 0 until numBanks) {
      d1Idx(b) := shadow(r1Idx(b))
      for (w <- 0 until ways) { d1Val(b * ways + w) := shadow(ageTab.io.qEnt(b)(w)(0)) }
    }
    // d1 拍有效的是 HT 桶字读的 UE —— 那是**组合**值（与 htNow 同源），
    // 不在这里捕获，见上面的 d1UeC / d2UeC 组合定义。

    // d1 → d2
    if (useKt) {
      d2V := shadow(d1V); d2Key := shadow(d1Key); d2Ad := shadow(d1Ad)
      d2Idx := shadow(d1Idx)
      d2OvH := shadow(d1OvH); d2OvS := shadow(d1OvS); d2OvP := shadow(d1OvP)
      d2Val := shadow(d1Val)
      // 用 d1 拍当拍的组合比对结果（此时 d1Val 已是本请求的 valid 位）
      d2FpOk := shadow(d1FpOkC); d2FpSel := shadow(d1FpSelC)
    } else if (useAd) {
      d2V := shadow(d1V); d2Hit := shadow(tblHit)
      d2FwH := shadow(fwdHit); d2FwAd := shadow(fwdAd)
      // d2 是这一支的 AD 数据拍，记下命中槽位供 UE 自愈定位（cmHit 保证槽位确实有效）
      adSBk := shadow(l.slotBank(cmSel)); adSWy := shadow(l.slotWay(cmSel))
      adSIdx := shadow(d1Idx(l.slotBank(cmSel)))
      adSValid := shadow(cmHit)
    }
    // d2 → d3
    if (useD3) {
      d3V := shadow(d2V); d3Hit := shadow(tblHit)
      d3FwH := shadow(fwdHit); d3FwAd := shadow(fwdAd)
      // d3 是 AD 数据拍：命中槽位来自 d2 拍的选择（d2FpOk 保证指纹命中路确实有效）
      adSBk := shadow(l.slotBank(d2FpSel)); adSWy := shadow(l.slotWay(d2FpSel))
      adSIdx := shadow(d2Idx(l.slotBank(d2FpSel)))
      adSValid := shadow(d2FpOk)
    }
    // UE 影子推进（与上面同一批边沿）
    shHtUe := shadow(d1UeC)               // d1 拍（组合）的 HT 读错 → 带到 d2/d3
    if (useD3) { d3UeAcc := shadow(d2UeC); d3UeSrc := shadow(d2UeSrcC) }
  }

  // =========================================================================
  // UE：结算（脉冲/计数/自愈）与 fail-safe
  // =========================================================================
  // 各拍的"本请求到目前为止是否读到过 UE"（组合；见上面影子寄存器的说明）
  private val d3UeC = d3UeAcc || d3AdC
  private val rspUe: Bool =
    if (useAd) (if (useKt) d3UeC else d2UeC)
    else cmUe
  private val rspUeSrc: UInt =
    if (useAd) (if (useKt) d3UeSrcC else d2UeSrcC)
    else (if (useKt) d2UeSrcC else EmUErrSrc.ht)

  // 每个请求/维护操作最多上报一次；查找侧在"响应那一拍"结算
  private val lkUeFireV: Bool =
    (if (useAd) (if (useKt) d3V && d3UeC else d2V && d2UeC) else cmV && cmUe) && adv

  // ---- UE 自愈：定位要作废的 HT 槽位 ----
  // HT 读错 → 只知道桶（桶字整体不可信）→ 作废该桶全部 way
  // KT 读错 → 指纹命中的那一路 → 作废那一路
  // AD 读错 → 发起 AD 读时选中的槽位（随流水带过来）→ 作废那一路
  // ⚠️ 必须 adSValid（该槽位确实来自 HT 槽位匹配）：OVFC 命中 / miss 时 adSBk/adSWy 是
  //    无效值，拿它作废会误删无关条目（见 adSValid 声明处）。
  private val invFireHt = d1UeC && !invBusy
  private val invFireKt = ktUeC && !invBusy
  private val invFireAd = (if (useD3) d3AdC else if (useAd) adUeC else false.B) &&
                          adSValid && !invBusy
  when(invFireHt) {
    val bk = PriorityEncoder(htUErrV.asUInt)(l.bankW - 1, 0)
    invReq(bk, d1Idx(bk), invAllWays)
  }.elsewhen(invFireKt) {
    invReq(l.slotBank(d2FpSel), d2Idx(l.slotBank(d2FpSel)), invOneWay(l.slotWay(d2FpSel)))
  }.elsewhen(invFireAd) {
    invReq(adSBk, adSIdx, invOneWay(adSWy))
  }

  // rsp.valid 必须"每个请求恰好一拍"：svc 抢时隙会把整条流水线冻结（adv=0），
  // 此时停在 d3 的请求会一直保持 valid —— 消费方按 valid 计数就会把一个请求算成多个响应。
  // 用 adv 门控：被冻结的那拍不报，解冻后补一拍（响应被顺延，但不会重复）。
  // UE 时：强制 miss 且 ad 清零 —— 绝不用坏数据当命中，也绝不把坏数据外传。
  if (useAd) {
    if (useKt) {
      io.rsp.valid := d3V && adv
      io.rsp.bits  := Cat((d3Hit || d3FwH) && !d3UeC,
                          Mux(d3UeC, 0.U(l.adW.W), Mux(d3FwH, d3FwAd, adMem.get.io.lgc.rdata)))
    } else {
      io.rsp.valid := d2V && adv
      io.rsp.bits  := Cat((d2Hit || d2FwH) && !d2UeC,
                          Mux(d2UeC, 0.U(l.adW.W), Mux(d2FwH, d2FwAd, adMem.get.io.lgc.rdata)))
    }
  } else {
    io.rsp.valid := cmV && adv
    // UE 时 ad 一并清零。注意 tblAdVal 在 useKt&&!useAd 时是 **KT 条目里内联的 ad**
    //（这一档没有 AD 表），不能因为 useKt 就当成 0。
    io.rsp.bits  := Cat(hit, Mux(fwdHit, fwdAd, Mux(cmUe, 0.U(l.adW.W), tblAdVal)))
  }

  // 上报：脉冲 + 来源（查找侧；svc 侧来自 SvcEngine 的 S_MEMERR）
  private val svcUeFire = eng.io.ueFire
  io.memUErr    := lkUeFireV || svcUeFire
  io.memUErrSrc := Mux(svcUeFire, eng.io.ueSrc, rspUeSrc)
  when(lkUeFireV || svcUeFire) { cntUErr := cntUErr + 1.U }

  // =========================================================================
  // 自学习转发 CAM 的弹出
  // =========================================================================
  if (learnOn) {
    // 转发 CAM 的弹出要**延迟 2 拍**，不能与 S_WR 同拍：
    //   HT 写在 T 拍落盘（T→T+1 边沿生效），所以在 T-1/T 拍发起 HT 读的请求拿到的是**旧数据**，
    //   它们的比较级分别在 T+1 / T+2 拍；若在 T 拍就弹出 CAM，这些请求会丢掉本应转发来的命中。
    //
    // ⚠️ 学习任务**插入失败时（S_ALLOC 分配不到）同样必须弹出**。不弹的话 head 一直不变、
    //    fwdPending 恒真，S_IDLE 会一次次启动同一个必然失败的任务 —— 只要表是满的，
    //    svc 引擎就会永远空转在这一个 key 上：既学不进任何东西（后面排队的 key 全被饿死），
    //    又因为 svcOwns 反复冻结流水线而拉低查找吞吐，insFail 还会一直涨到回绕。
    //    本次丢弃计入 cntInsFail（见引擎的 insFail 脉冲），不再另计 learnDrop（那不是 CAM 满）。
    val popPend = RegInit(false.B)
    val popCnt  = RegInit(0.U(3.W))
    when(eng.io.lrnPop) { popPend := true.B; popCnt := 2.U }
    .elsewhen(popPend) {
      when(popCnt === 0.U) { popPend := false.B }.otherwise { popCnt := popCnt - 1.U }
    }
    fwd.get.io.pop := popPend && (popCnt === 0.U)
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
  io.status.ovfcUse   := ovf.io.count
  io.status.fwdUse    := (if (learnOn) fwd.get.io.count else 0.U(fwdCntW.W))
  io.status.ktFree    := ktFree.map(f => f.io.count.asUInt).getOrElse(0.U)
  io.status.adFree    := adFree.map(f => f.io.count.asUInt).getOrElse(0.U)
  io.status.lkBusy    := acqV || r1V || d1V
  io.status.uerrCnt   := cntUErr
  // 作废还没清完时 svc 也算忙（否则上层以为空闲、下发命令又被上面 ready 挡住，来回试探）。
  // agPend（扫描器已 claim、等 svc 执行老化）同理：此时 wr.ready 也是 0，必须一起报忙。
  io.status.mtBusy    := eng.io.busy || invPend || agPend
}
