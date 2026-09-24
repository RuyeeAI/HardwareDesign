package em

import chisel3._
import chisel3.util._
import BaseCbb.memory.Bitmap

// ===========================================================================
// svc 共享访问引擎 —— 维护口 / 自学习插入 / 老化动作的状态机
//
// 从 ExactMatch 整体搬出（阶段3 重构）。搬的是「状态机 + 决策 + 选路/分配」，**存储实例
// 与读写地址 mux 留在 ExactMatch**：
//   · 引擎只输出「svc 侧」的读写请求（htRe/htRaddr、ktRe/ktRaddr、写口），
//     顶层用 `Mux(svcOwns, eng.*, 流水线)` 接进存储；
//   · AgeTable / OVFC 的写口与释放口由引擎直接驱动（实例在顶层）；
//   · **KT/AD 空闲池（memory/Bitmap）整个在引擎里**——分配/归还的时机完全由状态机决定，
//     放顶层只会多出 8 个"过一手"的端口，顶层只读 cnt；
//   · 计数与 entryCnt 留在顶层，引擎只给单拍脉冲（insDone/insFail/...）。
//
// 优先级：老化动作 > wr > 自学习插入。
// 每次操作至多 2 个"时隙"：① 读 HT 桶（+AgeTable 查询）② 指纹命中时读那 1 条 KT
//   时隙来源：① 流水线自然空档（零代价）② 等够 slotWaitMax 拍 → 反压上游并冻结流水线 1 拍
//   写不需要时隙：HT/KT/AD 都是 TP（同拍 1 读 + 1 写）
//
// ⚠️ 本模块是"忠实搬迁"，行为逐行等价，没有顺手改语义。下面所有 ⚠️ 注释都是原先
//    在 ExactMatch 里踩过的坑，务必保留。
// ===========================================================================
class SvcEngine(l: EmLayout, params: EmParams) extends Module {
  private val numBanks = l.numBanks
  private val ways     = l.ways
  private val ktBanks  = l.ktBanks
  private val keyW     = l.keyW
  private val adW      = l.adW
  private val useKt    = params.useKt
  private val useAd    = params.useAd
  private val fpW      = math.max(1, l.fpW)
  private val ovfcEn   = l.ovfcEn
  private val learnOn  = params.learning.isDefined
  private val useSerial = params.crc.isInstanceOf[CrcRuntime]
  private val payW     = math.max(1, l.ovfcPayW)
  private val selW     = l.ovfcCntW

  // 状态编码（与原先一致）
  private val T_WR  = 0.U(2.W); private val T_LRN = 1.U(2.W); private val T_AGE = 2.U(2.W)
  private val S_IDLE  = 0.U(4.W); private val S_HASH = 1.U(4.W); private val S_HTREQ = 2.U(4.W); private val S_HTW = 3.U(4.W)
  private val S_KTREQ = 4.U(4.W); private val S_KTW  = 5.U(4.W); private val S_DEC   = 6.U(4.W); private val S_ALLOC = 7.U(4.W)
  private val S_WR    = 8.U(4.W); private val S_FREE = 9.U(4.W); private val S_AGFR  = 10.U(4.W)
  private val S_HTD   = 11.U(4.W)
  private val S_MEMERR = 12.U(4.W)

  val io = IO(new Bundle {
    // ---- 时隙仲裁 ----
    val pipeFree = Input(Bool())
    val memReady = Input(Bool())

    // ---- 维护命令口 ----
    val wrValid = Input(Bool())
    val wrOp    = Input(UInt(2.W))
    val wrKey   = Input(UInt(keyW.W))
    val wrAd    = Input(UInt(adW.W))

    // ---- 自学习转发 head ----
    val fwdPend = Input(Bool())
    val fwdKey  = Input(UInt(keyW.W))
    val fwdAd   = Input(UInt(adW.W))

    // ---- 老化 pending 目标（来自 AgeSched）----
    val agPend  = Input(Bool())
    val agOv    = Input(Bool())
    val agOvSel = Input(UInt(selW.W))
    val agSlot  = Input(UInt(l.slotW.W))
    val agBk    = Input(UInt(l.bankW.W))
    val agIdx   = Input(UInt(l.idxW.W))
    val agWy    = Input(UInt(l.wayW.W))
    val agClaim = Input(Bool())
    val agPay   = Input(UInt(payW.W))

    // ---- OVFC 查询 ----
    val ovSvHit   = Input(Bool())
    val ovSvSel   = Input(UInt(selW.W))
    val ovSvPay   = Input(UInt(payW.W))
    val ovSvPayQ  = Input(UInt(payW.W))
    val ovHasFree = Input(Bool())
    val ovAlloc   = Input(UInt(selW.W))

    // ---- 存储回读 ----
    val htRdata = Input(Vec(numBanks, UInt(l.htWordW.W)))
    val ktRdata = Input(UInt(l.ktEntryW.W))
    val htQEnt  = Input(Vec(numBanks, Vec(ways, UInt(2.W))))
    val htUErr  = Input(Bool())
    val ktUErr  = Input(Bool())

    // ---- UE 自愈（寄存器与掩码留在顶层）----
    val invBusy = Input(Bool())
    val invBk   = Input(UInt(l.bankW.W))
    val invIdx  = Input(UInt(l.idxW.W))
    val invWy   = Input(UInt(l.wayW.W))
    val clrWasValid = Input(Bool())

    // ---- CRC 配置（仅 CrcRuntime 模式生效）----
    val crcPoly = Input(UInt(l.crcW.W))
    val crcInit = Input(UInt(l.crcW.W))
    val crcXor  = Input(UInt(l.crcW.W))

    // ==== 输出：svc 侧读请求（顶层用 svcOwns 与流水线 mux；写口直连）====
    val svcOwns = Output(Bool())
    val htRe    = Output(Bool())
    val htRaddr = Output(Vec(numBanks, UInt(l.idxW.W)))
    val ktRe    = Output(Bool())
    val ktRaddr = Output(UInt(l.ktPtrW.W))
    val htWe    = Output(Vec(numBanks, Bool()))
    val htWaddr = Output(Vec(numBanks, UInt(l.idxW.W)))
    val htWdata = Output(Vec(numBanks, UInt(l.htWordW.W)))
    val ktWe    = Output(Bool()); val ktWaddr = Output(UInt(l.ktPtrW.W)); val ktWdata = Output(UInt(l.ktEntryW.W))
    val adWe    = Output(Bool()); val adWaddr = Output(UInt(l.adPtrW.W)); val adWdata = Output(UInt(adW.W))

    // ==== 输出：AgeTable 写口（rf 仍由流水线驱动，不在这里）====
    val agInsEn = Output(Bool()); val agInsBk = Output(UInt(l.bankW.W)); val agInsIdx = Output(UInt(l.idxW.W)); val agInsWy = Output(UInt(l.wayW.W))
    val agClrEn = Output(Bool()); val agClrBk = Output(UInt(l.bankW.W)); val agClrIdx = Output(UInt(l.idxW.W)); val agClrWy = Output(UInt(l.wayW.W))

    // ==== 输出：OVFC 写 / 释放 / 按序号取 payload ====
    val ovWrEn = Output(Bool()); val ovWrSel = Output(UInt(selW.W)); val ovWrKey = Output(UInt(keyW.W))
    val ovWrPay = Output(UInt(payW.W)); val ovWrCntUp = Output(Bool())
    val ovFreeEn = Output(Bool()); val ovFreeSel = Output(UInt(selW.W))
    val ovSvPaySel = Output(UInt(selW.W))
    val ovMatchKey = Output(UInt(keyW.W))

    // ==== 输出：空闲池占用数（Bitmap 实例在引擎内，顶层 status 只读 cnt）====
    val ktCount = Output(UInt(math.max(1, log2Ceil(l.ktDepthReal + 1)).W))
    val adCount = Output(UInt(math.max(1, log2Ceil(params.adDepth + 1)).W))

    // ==== 输出：AgeSched 释放 ====
    val agRelease = Output(Bool())

    // ==== 输出：状态与单拍脉冲 ====
    val wrReady   = Output(Bool())
    val busy      = Output(Bool())
    val insDone   = Output(Bool())
    val insFail   = Output(Bool())
    val delDone   = Output(Bool())
    val learnDone = Output(Bool())
    val ageDrop   = Output(Bool())
    val fpClash   = Output(Bool())
    val entryInc  = Output(Bool())
    val entryDec  = Output(Bool())
    val ueFire    = Output(Bool())
    val ueSrc     = Output(UInt(2.W))
    val lrnPop    = Output(Bool())
    val invGo     = Output(Bool())
  })

  // =========================================================================
  // 空闲条目池（Bitmap ×2：KT / AD）—— 复用 memory/Bitmap（原 em/FreeList 与其
  // 功能重复，已删除：同为"1=可用、最低位优先分配、alloc 清 0 / free 置 1"的位图）
  //
  // 为什么放在引擎里：分配（S_ALLOC）与归还（S_FREE/S_AGFR）的时机完全由本状态机决定，
  // 放在顶层只会多出 8 个"过一手"的端口；顶层只读 cnt 聚合进 status。
  // ⚠️ 同拍 alloc+ret：Bitmap 是"分配清 0 生效"（clr 后写），EM 里二者分属 S_ALLOC 与
  //    S_FREE/S_AGFR，状态互斥，不可能同拍同址 —— 行为与旧 FreeList（归还优先）等价。
  // =========================================================================
  val ktFree: Option[Bitmap] = if (useKt) Some(Module(new Bitmap(l.ktDepthReal))) else None
  val adFree: Option[Bitmap] = if (useAd) Some(Module(new Bitmap(params.adDepth))) else None

  // =========================================================================
  // svc 寄存器
  // =========================================================================
  val sState = RegInit(S_IDLE)
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
  val sOvSel = Reg(UInt(selW.W))
  val sSlot  = Reg(UInt(l.slotW.W))
  val sKtPtr = Reg(UInt(l.ktPtrW.W))
  val sAdPtr = Reg(UInt(l.adPtrW.W))
  val sBest  = Reg(UInt(l.bankW.W))
  val sFreeW = Reg(UInt(l.wayW.W))
  val sUeSrc = Reg(UInt(2.W))
  val sRdw   = RegInit(0.U(math.max(1, log2Ceil(l.rdLat)).W))

  /** 维护：桶内与待查 key 同指纹且 valid 的槽位掩码（插入规则保证至多 1 位） */
  val sFpMatch = if (useKt) VecInit((0 until ktBanks).map(s => sVal(s) && l.htFp(sPay(s)) === sFp))
                 else VecInit(Seq(false.B))
  val sFpHit   = if (useKt) sFpMatch.asUInt.orR else false.B
  val sFpSel   = PriorityEncoder(sFpMatch)

  /** 老化 pending 目标的 claim 是否还在（见 S_HTD / S_IDLE 的说明） */
  val stillClm: Bool = Mux(io.agOv, io.agClaim, sClm(io.agSlot))

  /** 维护判定（组合）：桶内是否存在 Full Key 相等的条目（useKt 时只比指纹命中的那一条） */
  private val mFound: Bool =
    if (useKt) sFpHit && (l.ktKey(sKtE) === sKey)
    else VecInit((0 until ktBanks).map(s => sVal(s) && l.htKey(sPay(s)) === sKey)).asUInt.orR
  private val mSel: UInt =
    if (useKt) sFpSel
    else PriorityEncoder(VecInit((0 until ktBanks).map(s => sVal(s) && l.htKey(sPay(s)) === sKey)))

  // =========================================================================
  // 时隙仲裁：只有"读 HT 桶 / 读 KT"需要时隙；写不需要
  // =========================================================================
  val svcWait  = RegInit(0.U(math.max(1, log2Ceil(params.slotWaitMax + 1)).W))
  val svcWantC = (sState === S_HTREQ) || (sState === S_KTREQ)
  val svcForce = svcWantC && svcWait === params.slotWaitMax.U
  val svcOwns  = svcWantC && (io.pipeFree || svcForce)
  when(svcOwns) { svcWait := 0.U }
  .elsewhen(svcWantC) { when(svcWait =/= params.slotWaitMax.U) { svcWait := svcWait + 1.U } }

  io.svcOwns := svcOwns
  io.htRe    := sState === S_HTREQ
  io.htRaddr := sIdx
  val svcKtRAddr: UInt = if (useKt) l.htKtPtr(sPay(sSlot)) else 0.U(l.ktPtrW.W)
  io.ktRe    := sState === S_KTREQ
  io.ktRaddr := svcKtRAddr

  // =========================================================================
  // svc 主状态机
  // =========================================================================
  val fwdPending = if (learnOn) io.fwdPend else false.B
  // 作废请求在 S_IDLE 里优先于其它任务，所以它占着的时候不能收维护命令
  //（否则 ready 已经回了、命令却被丢）
  io.wrReady := (sState === S_IDLE) && !io.agPend && !io.invBusy

  /** svc 侧 OVFC 匹配的查询 key：S_HASH 阶段用已锁存的 sKey，否则用本拍要启动的任务 key
    *（S_IDLE 里命令优先于学习，与下面 when-elsewhen 链的优先级一致）。 */
  io.ovMatchKey :=
    Mux(sState === S_HASH, sKey,
      Mux(io.wrValid, io.wrKey, if (learnOn) io.fwdKey else 0.U))

  private def hardHash(d: UInt): UInt = {
    val c = params.crc
    Crc.hardwired(d, l.crcW, c.poly, c.init, c.xorout, c.refin, c.refout)
  }
  private def slices(h: UInt): Vec[UInt] =
    VecInit((0 until numBanks).map(b => h(math.min(l.crcW - 1, (b + 1) * l.idxW - 1), b * l.idxW)))

  private val svCrc = if (useSerial) Some(Module(new CrcSerial(l.crcW, keyW, params.crc.refin, params.crc.refout))) else None
  svCrc.foreach { m =>
    m.io.start := false.B
    m.io.din   := 0.U
    m.io.poly  := io.crcPoly
    m.io.init  := io.crcInit
    m.io.xor   := io.crcXor
  }

  /** 启动一次维护/学习任务：命中 OVFC 就直接用它的 KT 指针，否则先读 HT 桶 */
  private def startTask(k: UInt, a: UInt, op: UInt, task: UInt): Unit = {
    sKey := k; sAd := a; sOp := op; sTask := task
    sUseOv := io.ovSvHit; sOvSel := io.ovSvSel
    if (useSerial) { svCrc.get.io.start := true.B; svCrc.get.io.din := k; sState := S_HASH }
    else {
      val h = hardHash(k)
      sIdx := slices(h); sFp := l.fpOf(h)
      when(io.ovSvHit) {
        sKtPtr := io.ovSvPay
        sState := Mux(useKt.B, S_KTREQ, S_DEC)
      }.otherwise {
        sState := S_HTREQ
      }
    }
  }

  // UE 自愈的执行：作废请求优先于其它 svc 任务
  val invGo = (sState === S_IDLE) && io.invBusy
  io.invGo := invGo

  when(sState === S_IDLE) {
    when(!io.invBusy) {
      when(io.agPend) {
        // ⚠️ 启动老化任务前先确认 OVFC 项的 claim 还在：扫描器 claim 之后，可能有**先启动的**
        //    维护任务动过这一项（删除会顺带清 claim 并归还 KT/AD；add/upd 覆盖会清 claim 并
        //    刷新时间戳）。此时再老化一次就是**双重释放** / 删掉刚刷新的条目。
        //    claim 已丢 ⇒ 放弃本次老化任务（不释放、不减计数），放开 agPend 让扫描器继续。
        //    HT 槽位那一支的同类确认放在 S_HTD（要等桶数据读回来才能看），见那里。
        val ovClaimGone: Bool = io.agOv && !io.agClaim
        sTask := T_AGE
        sUseOv := io.agOv
        sOvSel := io.agOvSel
        when(ovClaimGone) {
          // 目标已被维护路径处理完 → 本次老化放弃；agPend 由 agSched.io.release 放掉
        }.elsewhen(io.agOv) {
          sKtPtr := io.agPay
          sState := Mux(useKt.B, S_KTREQ, S_AGFR)
        }.otherwise {
          for (b <- 0 until numBanks) { sIdx(b) := io.agIdx }
          sState := S_HTREQ
        }
      }.elsewhen(io.wrValid) {
        startTask(io.wrKey, io.wrAd, io.wrOp, T_WR)
      }.elsewhen(if (learnOn) fwdPending else false.B) {
        if (learnOn) { startTask(io.fwdKey, io.fwdAd, EmOp.add, T_LRN) }
      }
    }
  }
  if (useSerial) {
    when(sState === S_HASH) {
      when(svCrc.get.io.done) {
        val h = svCrc.get.io.out
        sIdx := slices(h); sFp := l.fpOf(h)
        sUseOv := io.ovSvHit; sOvSel := io.ovSvSel
        when(io.ovSvHit) { sKtPtr := io.ovSvPay; sState := Mux(useKt.B, S_KTREQ, S_DEC) }
        .otherwise { sState := S_HTREQ }
      }
    }
  }

  // =========================================================================
  // svc 读捕获（S_HTREQ/S_KTREQ 发起，等满 rdLat 拍后捕获）
  // =========================================================================
  private def svcRdDone: Bool = sRdw === (l.rdLat - 1).U
  // UE 判定必须卡在"捕获那一拍"：等待期间 htUErr/ktUErr 属于别的（查找）读，不能拿来判 svc
  val svcCapHt = (sState === S_HTW) && svcRdDone
  val svcCapKt = useKt.B && (sState === S_KTW) && svcRdDone
  val svcUeHt = svcCapHt && io.htUErr
  val svcUeKt = svcCapKt && io.ktUErr

  when(svcOwns && sState === S_HTREQ) {
    sState := S_HTW; sRdw := 0.U
    for (b <- 0 until numBanks) {
      for (w <- 0 until ways) {
        sVal(b * ways + w) := io.htQEnt(b)(w)(0)
        sClm(b * ways + w) := io.htQEnt(b)(w)(1)
      }
    }
  }
  when(svcOwns && sState === S_KTREQ) { sState := S_KTW; sRdw := 0.U }

  when(sState === S_HTW) {
    when(svcRdDone) {
      for (b <- 0 until numBanks) {
        val rd = io.htRdata(b).asTypeOf(Vec(ways, UInt(l.htPayW.W)))
        for (w <- 0 until ways) { sPay(b * ways + w) := rd(w) }
      }
      sState := S_HTD      // 决策放到下一拍，此时 sPay 已是本桶数据
    }.otherwise { sRdw := sRdw + 1.U }
  }

  // ---- S_HTD：用刚捕获的桶数据（sPay/sVal）做判断 ----
  // ⚠️ 必须独立于 S_HTW：sPay 是 S_HTW 当拍末才写入的，若在 S_HTW 当拍就用
  //    sFpHit/sFpSel 判断，拿到的是**上一轮**的桶数据。
  when(sState === S_HTD) {
    when(sTask === T_AGE) {
      // ⚠️ 必须重新确认"claim 还在"。扫描器 claim 之后、本任务读到 HT 之前，可能已有
      //    维护操作动过同一个槽位：
      //      · 删除：clrEn 会顺带清 claim 并归还 KT/AD —— 此处若不再确认就再归还一次，
      //        就是**双重释放**（free list 出现重复地址 → 后续 alloc 撞车 → 静默数据损坏）；
      //      · add/upd 覆盖：insEn 会清 claim 并刷新 ts —— 此处若照常老化，会把刚刷新的
      //        条目删掉，且 entryCnt 多减一次。
      //    claim 已丢 ⇒ 该条目已被维护路径处理完，本次老化任务整体放弃：不释放、不减计数、
      //    直接回 IDLE（agPend 由 agSched.io.release 放掉，让扫描器继续）。
      when(!stillClm) {
        sState := S_IDLE
      }.otherwise {
        // 老化：拿被 claim 那一槽的 payload → KT 指针
        sSlot  := io.agSlot
        sKtPtr := (if (useKt) l.htKtPtr(sPay(io.agSlot)) else 0.U(l.ktPtrW.W))
        sState := Mux(useKt.B, S_KTREQ, S_AGFR)
      }
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
      when(svcRdDone) {
        sKtE := io.ktRdata
        sState := Mux(sTask === T_AGE, S_AGFR, S_DEC)
      }.otherwise { sRdw := sRdw + 1.U }
    }
  }

  // ---- svc 侧 UE：读 HT/KT 拿到不可纠错误 → 放弃本次任务（绝不拿坏数据写回）----
  // 放在 S_HTW / S_KTW 的赋值之后，用后面的赋值覆盖它们的状态转移。
  when(svcUeHt || svcUeKt) {
    sUeSrc := Mux(svcUeHt, EmUErrSrc.ht, EmUErrSrc.kt)
    sState := S_MEMERR
  }

  // ---- S_MEMERR：把可能已 claim 的槽位放掉，然后回 IDLE（任务整体放弃）----
  // · 老化任务：claim 已置 → 清 valid + claim（条目本就过期/损坏，顺手作废，能自愈）
  // · 维护任务：没有 claim → 只回 IDLE（命令静默失败，靠 memUErr 脉冲告知）
  // · OVFC 项：清 ovfcV/ovfcC 并减计数
  // ⚠️ 不归还 KT/AD：指针来自坏数据（sPay/sKtE 不可信），宁可有界泄漏也不乱释放。
  val memErrSt   = sState === S_MEMERR
  // ⚠️ 必须限定"当前任务确实是老化任务"（sTask === T_AGE）。原来只判 agPend 是错的：
  //    维护任务在途时扫描器也可能已 claim（agPend=1），此时维护任务读到 UE 会误把**扫描器
  //    的 claim 槽位**清掉 —— 那是一个合法条目被无效化、且它的 KT/AD 永远不会归还（泄漏）。
  //    维护任务自己没有 claim，遇 UE 只需回 IDLE（命令静默失败，靠 memUErr 脉冲告知）。
  val abortClrHt = memErrSt && io.agPend && !io.agOv && (sTask === T_AGE)
  val abortClrOv = memErrSt && io.agPend && io.agOv && (sTask === T_AGE)
  when(memErrSt) {
    sState := S_IDLE
  }

  // ---- 维护判定 ----
  when(sState === S_DEC) {
    when(sUseOv) {
      sFound := true.B
      if (useAd) {
        val a = if (useKt) l.ktAdPtr(sKtE)
                else if (ovfcEn) l.htAdPtr(io.ovSvPayQ)
                else 0.U(1.W)
        sAdPtr := a
      }
      sState := Mux(sOp === EmOp.del, S_FREE, S_WR)
    }.elsewhen(mFound) {
      sFound := true.B
      if (useKt) { sKtPtr := l.htKtPtr(sPay(sFpSel)) }
      sSlot := mSel
      if (useAd) {
        val a = if (useKt) l.ktAdPtr(sKtE) else l.htAdPtr(sPay(mSel))
        sAdPtr := a
      }
      sState := Mux(sOp === EmOp.del, S_FREE, S_WR)
    }.elsewhen(sOp === EmOp.del) {
      sFound := false.B
      sState := S_IDLE
    }.elsewhen(sOp === EmOp.upd) {
      sFound := false.B
      sState := S_IDLE
    }.otherwise {
      sFound := false.B
      sState := S_ALLOC
    }
  }

  // ---- d-left 选路 + 空闲池（Bitmap）分配 ----
  // 插入规则：候选槽位里已有同指纹条目（sFpHit）时，整个 HT 都不能放这条 key，
  // 否则后续查找会撞上别人的指纹 → 直接落 OVFC（OVFC 用全 key 比较，精确）。
  val allocWill = Wire(Bool())
  allocWill := false.B
  /** S_ALLOC 真的能分配下去：HT 或 OVFC 有位，且 KT/AD 有空闲。
    * ⚠️ 语义映射：Bitmap 的 `full = 无可用位`（`empty = 池空即全可用`），"有空闲" = `!full`。 */
  private val sAllocOk: Bool =
    allocWill && ktFree.map(f => !f.io.full).getOrElse(true.B) &&
      adFree.map(f => !f.io.full).getOrElse(true.B)
  when(sState === S_ALLOC) {
    val fpClash = sFpHit

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
    if (ovfcEn) { when(!htFree) { sOvSel := io.ovAlloc } }

    allocWill := htFree || io.ovHasFree

    when(sAllocOk) {
      ktFree.foreach(f => sKtPtr := f.io.req_ptr)
      adFree.foreach(f => sAdPtr := f.io.req_ptr)
      sState := S_WR
    }.otherwise {
      sState := S_IDLE
    }
  }
  // ⚠️ 分配要带 !full 门控（与旧 FreeList 的 alloc && okNow 等价）：Bitmap 满时若仍拉
  //    req_vld，会去清 req_ptr(=0) 那一位 —— 已分配的资源被误标成可用，后续重复分配。
  ktFree.foreach(f => f.io.req_vld := (sState === S_ALLOC) && allocWill && !f.io.full)
  adFree.foreach(f => f.io.req_vld := (sState === S_ALLOC) && allocWill && !f.io.full)

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
    io.htWe(b)    := sel
    io.htWaddr(b) := sIdx(b)
    io.htWdata(b) := VecInit((0 until ways).map(w => Mux(wrWay === w.U, wrPay, sPay(b * ways + w)))).asUInt
  }

  io.ktWe    := useKt.B && wrEn && (!sFound || !useAd.B)
  io.ktWaddr := sKtPtr
  io.ktWdata := l.ktEntry(sKey, (if (useAd) sAdPtr else sAd))

  io.adWe    := useAd.B && wrEn
  io.adWaddr := sAdPtr
  io.adWdata := sAd

  io.agInsEn  := wrEn && !sUseOv
  io.agInsBk  := wrBank
  io.agInsIdx := sIdx(wrBank)
  io.agInsWy  := wrWay

  // OVFC 写入（HT 放不下时落到这里）
  io.ovWrEn     := wrEn && sUseOv
  io.ovWrSel    := sOvSel
  io.ovWrKey    := sKey
  io.ovWrPay    := (if (useKt) sKtPtr else if (useAd) sAdPtr else sAd)
  io.ovWrCntUp  := !sFound

  // =========================================================================
  // 归还：删除 / 老化（归还 KT/AD 与清 valid 同拍完成）
  // =========================================================================
  val freeKey = (sState === S_FREE) && sFound
  val agFr    = sState === S_AGFR
  val relEn   = freeKey || agFr

  private val ktRelPtr: UInt =
    if (!useKt) 0.U(l.ktPtrW.W)
    else Mux(agFr, l.htKtPtr(sPay(io.agSlot)), sKtPtr)
  private val adRelPtr: UInt =
    if (!useAd) 0.U(l.adPtrW.W)
    else {
      // 老化任务要归还的 AD 指针：
      //   useKt  → 从 KT 条目里取（OVFC 的 payload 此时存的是 KT 指针，不是 AD 指针！）
      //   !useKt → OVFC 命中时 payload 直接就是 adPtr；否则从 HT 桶里取
      val agAdRel: UInt =
        if (useKt) l.ktAdPtr(sKtE)
        else Mux(io.agOv, (if (ovfcEn) io.agPay else 0.U(l.adPtrW.W)), l.htAdPtr(sPay(io.agSlot)))
      Mux(agFr, agAdRel, sAdPtr)
    }

  ktFree.foreach { f => f.io.ret_vld := relEn; f.io.ret_ptr := ktRelPtr }
  adFree.foreach { f => f.io.ret_vld := relEn; f.io.ret_ptr := adRelPtr }

  // clr 的三类来源：删除/老化完成、svc 遇 UE 放弃（顺带作废）、UE 自愈作废
  // ⚠️ 优先级：clr 高于 clm/ins（AgeTable 内部实现），作废与 claim 都走这一支。
  io.agClrEn  := (freeKey && !sUseOv) || (agFr && !io.agOv) || abortClrHt || invGo
  io.agClrBk  := Mux(invGo, io.invBk, Mux(agFr || abortClrHt, io.agBk, l.slotBank(sSlot)))
  io.agClrIdx := Mux(invGo, io.invIdx, Mux(agFr || abortClrHt, io.agIdx, sIdx(l.slotBank(sSlot))))
  io.agClrWy  := Mux(invGo, io.invWy, Mux(agFr || abortClrHt, io.agWy, l.slotWay(sSlot)))

  // 老化 pending 目标的释放（放掉后扫描器才能 claim 下一个）：
  //   · 老化任务做完了（S_AGFR）
  //   · 老化任务读到 UE 放弃（S_MEMERR && T_AGE）
  //   · S_HTD 发现 claim 已被维护路径清掉
  //   · S_IDLE 启动老化前就发现 OVFC 项的 claim 已丢
  // 与 claim 的先后：release 的各来源都要求 pend 已为 1，而 claim 要求 pend=0，二者不可能同拍
  //（AgeSched 里还额外取了 claim 优先作为保险）。
  io.agRelease := agFr ||
    (memErrSt && (sTask === T_AGE)) ||
    ((sState === S_HTD) && (sTask === T_AGE) && !stillClm) ||
    ((sState === S_IDLE) && !io.invBusy && io.agOv && !io.agClaim)

  // OVFC 释放：三条来源（删除命中的 OVFC 项 / 老化完成 / UE 放弃）——同一拍只可能有一条
  io.ovFreeEn  := (freeKey && sUseOv) || (agFr && io.agOv) || abortClrOv
  io.ovFreeSel := Mux(freeKey && sUseOv, sOvSel, io.agOvSel)

  // 维护任务路径按序号取 OVFC payload（S_DEC 用；与老化路径的 agSel 分开，避免同拍打架）
  io.ovSvPaySel := sOvSel

  when(relEn) {
    sFound := false.B
    sState := S_IDLE
  }

  // =========================================================================
  // 统计脉冲（计数实体留在顶层）
  // =========================================================================
  // 插入失败：S_DEC 的 upd 未命中 / S_ALLOC 分配不到
  io.insFail := ((sState === S_DEC) && !sUseOv && !mFound && (sOp === EmOp.upd)) ||
                ((sState === S_ALLOC) && !sAllocOk)
  io.fpClash := (sState === S_ALLOC) && sFpHit
  io.delDone := relEn && !agFr
  io.ageDrop := relEn && agFr
  io.insDone := (sState === S_WR) && (sTask =/= T_LRN)
  io.learnDone := (sState === S_WR) && (sTask === T_LRN)
  io.entryInc := (sState === S_WR) && !sFound
  // 条目数减少：归还（删除/老化）、S_MEMERR 放弃老化时的作废、UE 自愈作废
  io.entryDec := relEn ||
    (abortClrHt && io.clrWasValid) || abortClrOv ||
    (invGo && io.clrWasValid)

  // 学习任务在 S_WR 落盘后、或 S_ALLOC 分配失败后，都要弹转发 CAM 的 head
  io.lrnPop := learnOn.B && (sTask === T_LRN) &&
    ((sState === S_WR) || ((sState === S_ALLOC) && !sAllocOk))

  when(sState === S_WR) { sState := S_IDLE }

  io.busy   := sState =/= S_IDLE
  io.ueFire := sState === S_MEMERR
  io.ueSrc  := sUeSrc

  // 空闲池可用数（未启用 KT/AD 的那一路恒 0；顶层 status 直读）
  io.ktCount := ktFree.map(_.io.cnt).getOrElse(0.U(math.max(1, log2Ceil(l.ktDepthReal + 1)).W))
  io.adCount := adFree.map(_.io.cnt).getOrElse(0.U(math.max(1, log2Ceil(params.adDepth + 1)).W))
}
