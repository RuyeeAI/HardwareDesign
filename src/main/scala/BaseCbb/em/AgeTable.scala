package em

import chisel3._
import chisel3.util._

// ===========================================================================
// HT 老化信息表 —— valid/claim 寄存器阵列 + 时间戳 SRAM + 后台扫描器
//
// 分两块存：
//   · **valid / claim**：寄存器阵列（2 位/条），查找与维护都要在**同拍组合**读到它
//     （d1 的指纹比对要 valid，S_HTD 的选路要 valid+claim），所以必须在寄存器里。
//   · **时间戳 ts**：只被后台扫描器使用，查找/维护完全不碰 → 放**独立 SRAM**
//     （ageWords × ageW 位）。逐条 ts 用寄存器是 ageWords×ageW 个 flop（2left 预设
//     4×1024×16 = 65536 flop，比 HT SRAM 还大），换成一块 SRAM 面积降约 3 倍。
//
// 为什么 ts 不塞回 HT SRAM：那样扫描就得读 HT 的读端口，只能做成"机会式扫描"
//   （等查找流水线有空档才读）——**拿查找带宽换面积**，II=1 无法保证。
//   独立 SRAM 有自己的读写端口，扫描读它不占 HT/KT/AD 的任何带宽，
//   所以"扫描不占访存带宽"这个设计目标完整保留。
//
// 扫描器与 ts 回读的对齐：ts SRAM 是 rdLat=1（本拍给 re/raddr，下拍 tsRd 有效），
//   所以扫描器维护一条**延迟 1 拍的游标** dCur*，用它与回读的 ts 配对做判定
//   （见下面 cur* / dCur* 的说明）。
//
// 代价：htDepth*ways*2 个 flop（2left 预设约 8k flop）+ 一块 ageWords×ageW 的 SRAM。
//   真做硅时这 2 位还可以进一步省：claim 只对"正在老化"的少数条目有意义，
//   可以换成一个小 CAM（正在老化的槽位列表）。
//
// claim 位是"老化互斥"的关键：
//   扫描器认定某条过期 → 先置 claim（**不清 valid**，查找仍可命中它）→
//   申请时隙读 HT 拿 KT 指针 → 申请时隙读 KT 拿 AD 指针 →
//   同拍归还 KT/AD 指针 + 清 valid + 清 claim。
//   维护插入的"选空闲路"必须把 claim 当占用（不会复用正在老化的路），
//   维护删除遇到 claim 必须跳过释放（交给扫描器），
//   二者目标集合天然互斥（扫描器只碰 valid、插入只挑 invalid），因此不会二次压栈。
//
// 写优先级：
//   · valid/claim 阵列：clr > clm > ins（rf 不再写这一块，见下）
//   · ts SRAM：ins 与 rf 写的都是 `now`（同值），不需要优先级
// ⚠️ 刷新（rf）**只写 ts、不写 valid/claim**。原实现给 rf 也写了 valid=1，那是个隐患：
//   查找读到某槽位有效之后、刷新写回之前，该槽位若被老化清除并重新插入成**另一个 key**，
//   刷新就会把一个不属于它的槽位标成 valid → 假命中。ts 独立成 SRAM 后这个隐患自然消失。
// ===========================================================================

class AgeTable(l: EmLayout, timeout: BigInt) extends Module {
  val entW   = l.ageRegW          // valid + claim（时间戳在独立 SRAM）
  val ageW   = l.ageW
  val tsW    = math.max(1, ageW)
  val idxWid = math.max(1, log2Ceil(l.ageWords))

  val io = IO(new Bundle {
    // ---- 时间 ----
    val now = Input(UInt(l.ageW1.W))

    // ---- 时间戳 SRAM 读写口（rdLat = 1：本拍给 re/raddr，下拍 tsRd 有效）----
    val tsRd    = Input(UInt(tsW.W))
    val tsRaddr = Output(UInt(l.ageTsAddrW.W))
    val tsRe    = Output(Bool())
    val tsWaddr = Output(UInt(l.ageTsAddrW.W))
    val tsWe    = Output(Bool())
    val tsWdata = Output(UInt(tsW.W))

    // ---- 扫描器 ----
    val scanEn   = Input(Bool())                     // 扫描使能
    val scanHold = Input(Bool())                     // 已有 claim 在处理 → 暂停推进
    val scanHit  = Output(Bool())                    // 游标处命中"可老化条目"（由外部同拍 claim）
    val scanBk   = Output(UInt(l.bankW.W))
    val scanIdx  = Output(UInt(l.idxW.W))
    val scanWy   = Output(UInt(l.wayW.W))

    // ---- 桶查询：查找流水线 / 维护选路共用（svc 占时隙时由维护驱动）----
    // 只对外暴露 {claim, valid} 两位：时间戳不参与查找/维护判定，没必要把它也拉出这棵
    // htDepth*ways:1 的组合 mux。布局：bit0 = valid，bit1 = claim。
    val qIdx = Input(Vec(l.numBanks, UInt(l.idxW.W)))
    val qEnt = Output(Vec(l.numBanks, Vec(l.ways, UInt(2.W))))

    // ---- 写口 ----
    val insEn  = Input(Bool())                       // 插入：valid=1, claim=0, ts=now
    val insBk  = Input(UInt(l.bankW.W)); val insIdx = Input(UInt(l.idxW.W)); val insWy = Input(UInt(l.wayW.W))
    val rfEn   = Input(Bool())                       // 命中刷新：只写 ts=now（不动 valid/claim）
    val rfBk   = Input(UInt(l.bankW.W)); val rfIdx  = Input(UInt(l.idxW.W)); val rfWy  = Input(UInt(l.wayW.W))
    val clrEn  = Input(Bool())                       // 清空：valid=0, claim=0
    val clrBk  = Input(UInt(l.bankW.W)); val clrIdx = Input(UInt(l.idxW.W)); val clrWy = Input(UInt(l.wayW.W))
    // 本拍 clrEn 命中的条目原本是不是 valid —— UE 自愈作废时要靠它同步减条目数
    //（只减真的清掉了的；否则 entries 会虚高）
    val clrWasValid = Output(Bool())
    val clmEn  = Input(Bool())                       // 置 claim（保留 valid）
                                                 // ⚠️ claim 只由扫描器使用，目标恒为扫描游标处那条，
                                                 //    所以没有地址端口 —— 直接复用 dCur*（见写口）
  })

  // ---- 条目字段访问器 ----
  private def eValid(e: UInt): Bool = e(0)
  private def eClaim(e: UInt): Bool = e(1)
  private def mkEnt(v: Bool, c: Bool): UInt = Cat(c, v)

  private val perBank = l.bankDepth * l.ways
  /** (bank, idx, way) → 扁平条目号 */
  private def flat(b: UInt, i: UInt, w: UInt): UInt =
    ((b * perBank.U) + (i * l.ways.U) + w)(idxWid - 1, 0)

  // 全部初始化为"无效"（对应 Memory 的 initValue = AllZero）
  val ents = RegInit(VecInit(Seq.fill(l.ageWords)(0.U(entW.W))))

  // =========================================================================
  // 扫描器：游标自增，发现 valid && !claim && 过期 即报告
  //
  // ts 在 SRAM 里、回读要 1 拍，所以判定用的条目号是**上一拍**的游标（dCur*）：
  //   本拍：tsRaddr = flat(cur*)，tsRe = adv（adv = scanEn && !scanHold）
  //   下拍：tsRd 已经是 cur* 那一条的 ts，dCur* 也正好是 cur* → 三者配对，做判定
  // 于是"发起读"和"推进游标"必须同拍发生（都在 adv 时），否则配不上。
  // =========================================================================
  val curB = RegInit(0.U(l.bankW.W))
  val curI = RegInit(0.U(l.idxW.W))
  val curW = RegInit(0.U(l.wayW.W))
  val dCurB = RegInit(0.U(l.bankW.W))   // 延迟 1 拍（与 tsRd 对齐）
  val dCurI = RegInit(0.U(l.idxW.W))
  val dCurW = RegInit(0.U(l.wayW.W))

  val adv = io.scanEn && !io.scanHold
  val dAdv = RegNext(adv, false.B)      // 上一拍真的发起了读（扫到一半停下来的那拍不算）

  io.tsRaddr := flat(curB, curI, curW)
  io.tsRe    := adv
  when(adv) {
    dCurB := curB; dCurI := curI; dCurW := curW
    when(curW === (l.ways - 1).U) {
      curW := 0.U
      when(curI === (l.bankDepth - 1).U) {
        curI := 0.U
        when(curB === (l.numBanks - 1).U) { curB := 0.U }.otherwise { curB := curB + 1.U }
      }.otherwise { curI := curI + 1.U }
    }.otherwise { curW := curW + 1.U }
  }

  val dE = ents(flat(dCurB, dCurI, dCurW))
  // 未启用老化（ageW=0）时恒判不过期；不能拿 timeout=0 兜底，否则全表被删
  val curExp = if (ageW > 0) (io.now - io.tsRd) >= timeout.U(l.ageW1.W) else false.B

  io.scanHit := dAdv && !io.scanHold && eValid(dE) && !eClaim(dE) && curExp
  io.scanBk  := dCurB
  io.scanIdx := dCurI
  io.scanWy  := dCurW

  // =========================================================================
  // 桶查询（组合读，供查找流水线 / 维护选路使用）
  // =========================================================================
  io.qEnt := VecInit((0 until l.numBanks).map { b =>
    VecInit((0 until l.ways).map { w =>
      val e = ents(flat(b.U(l.bankW.W), io.qIdx(b), w.U(l.wayW.W)))
      Cat(eClaim(e), eValid(e))
    })
  })

  // =========================================================================
  // 写口
  // =========================================================================
  val insIdxE = flat(io.insBk, io.insIdx, io.insWy)
  val rfIdxE  = flat(io.rfBk, io.rfIdx, io.rfWy)
  val clrIdxE = flat(io.clrBk, io.clrIdx, io.clrWy)
  val clmIdxE = flat(dCurB, dCurI, dCurW)   // claim 的目标 = 扫描游标处（见 io.clmEn 注释）

  // ---- valid/claim 阵列：clr > clm > ins（目标集合互斥，顺序其实无所谓）----
  val entsWrEn  = io.clrEn || io.clmEn || io.insEn
  val entsWrIdx = Mux(io.clrEn, clrIdxE, Mux(io.clmEn, clmIdxE, insIdxE))
  val entsWrEnt = Mux(io.clrEn, mkEnt(false.B, false.B),
                  Mux(io.clmEn, mkEnt(true.B,  true.B),
                  /* ins */    mkEnt(true.B,  false.B)))
  when(entsWrEn) { ents(entsWrIdx) := entsWrEnt }

  // ---- ts SRAM：ins（插入）与 rf（刷新）都是 ts := now，同拍同值，不需要优先级 ----
  io.tsWaddr := Mux(io.insEn, insIdxE, rfIdxE)
  io.tsWdata := io.now
  if (ageW > 0) { io.tsWe := io.insEn || io.rfEn } else { io.tsWe := false.B }

  io.clrWasValid := eValid(ents(clrIdxE))
}