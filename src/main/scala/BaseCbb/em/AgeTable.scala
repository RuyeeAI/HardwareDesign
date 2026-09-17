package em

import chisel3._
import chisel3.util._

// ===========================================================================
// HT 老化信息表 —— (valid, 时间戳, claim) 寄存器阵列 + 后台扫描器
//
// 为什么单独放寄存器而不是塞进 HT SRAM：
//   老化扫描必须能在**不占用访存带宽**的前提下发现"可老化条目"，
//   否则扫描本身会和查找流水线抢读端口，II=1 无法保持。
//   放进寄存器后，扫描是纯组合读 + 游标自增，零带宽；
//   只有"确定要老化某条"之后，才去申请时隙读 HT 取 KT 指针、读 KT 取 AD 指针。
//
// 代价：htDepth*ways*(1+ageW+1) 个 flop（basic 预设约 74k flop），比 HT SRAM 本身还大。
//   真做硅时这里和 FreeList 一样属于行为级建模，应换成 bitmap + 优先编码器
//   （或者把时间戳放回 SRAM + 机会式扫描，用带宽换面积）。
//
// claim 位是"老化互斥"的关键：
//   扫描器认定某条过期 → 先置 claim（**不清 valid**，查找仍可命中它）→
//   申请时隙读 HT 拿 KT 指针 → 申请时隙读 KT 拿 AD 指针 →
//   同拍归还 KT/AD 指针 + 清 valid + 清 claim。
//   维护插入的"选空闲路"必须把 claim 当占用（不会复用正在老化的路），
//   维护删除遇到 claim 必须跳过释放（交给扫描器），
//   二者目标集合天然互斥（扫描器只碰 valid、插入只挑 invalid），因此不会二次压栈。
//
// 写优先级：clr > clm > ins > rf
//   - clr（老化完成 / 删除）最优先：保证不会留下"valid 但 KT 已归还"的悬挂条目
//   - clm 高于 rf：同拍"命中刷新"与"claim"时 claim 必须保住（刷新只写 ts，丢了也无所谓，
//     因为该条目马上要被老化掉）
//   - ins 与 clm 目标集合互斥（ins 只挑 invalid 路），顺序无所谓
// ===========================================================================

class AgeTable(l: EmLayout, timeout: BigInt) extends Module {
  val entW   = l.ageEntryW                                   // valid + ts + claim
  val ageW   = l.ageW
  val idxWid = math.max(1, log2Ceil(l.ageWords))

  val io = IO(new Bundle {
    // ---- 时间 ----
    val now = Input(UInt(l.ageW1.W))

    // ---- 扫描器 ----
    val scanEn   = Input(Bool())                     // 扫描使能
    val scanHold = Input(Bool())                     // 已有 claim 在处理 → 暂停推进
    val scanHit  = Output(Bool())                    // 游标处命中"可老化条目"（由外部同拍 claim）
    val scanBk   = Output(UInt(l.bankW.W))
    val scanIdx  = Output(UInt(l.idxW.W))
    val scanWy   = Output(UInt(l.wayW.W))

    // ---- 桶查询：查找流水线 / 维护选路共用（svc 占时隙时由维护驱动）----
    val qIdx = Input(Vec(l.numBanks, UInt(l.idxW.W)))
    val qEnt = Output(Vec(l.numBanks, Vec(l.ways, UInt(entW.W))))

    // ---- 写口 ----
    val insEn  = Input(Bool())                       // 插入：valid=1, ts=now, claim=0
    val insBk  = Input(UInt(l.bankW.W)); val insIdx = Input(UInt(l.idxW.W)); val insWy = Input(UInt(l.wayW.W))
    val rfEn   = Input(Bool())                       // 命中刷新：ts=now（不动 valid，**不复活条目**）
    val rfBk   = Input(UInt(l.bankW.W)); val rfIdx  = Input(UInt(l.idxW.W)); val rfWy  = Input(UInt(l.wayW.W))
    val clrEn  = Input(Bool())                       // 清空：valid=0, claim=0
    val clrBk  = Input(UInt(l.bankW.W)); val clrIdx = Input(UInt(l.idxW.W)); val clrWy = Input(UInt(l.wayW.W))
    val clmEn  = Input(Bool())                       // 置 claim（保留 valid / ts）
    val clmBk  = Input(UInt(l.bankW.W)); val clmIdx = Input(UInt(l.idxW.W)); val clmWy = Input(UInt(l.wayW.W))
  })

  // ---- 条目字段访问器 ----
  private def eValid(e: UInt): Bool = e(0)
  private def eTs(e: UInt): UInt = if (ageW > 0) e(ageW, 1) else 0.U(1.W)
  private def eClaim(e: UInt): Bool = e(1 + ageW)
  private def mkEnt(v: Bool, ts: UInt, c: Bool): UInt =
    if (ageW > 0) Cat(c, ts(ageW - 1, 0), v) else Cat(c, v)

  private val perBank = l.bankDepth * l.ways
  /** (bank, idx, way) → 扁平条目号 */
  private def flat(b: UInt, i: UInt, w: UInt): UInt =
    ((b * perBank.U) + (i * l.ways.U) + w)(idxWid - 1, 0)

  // 全部初始化为"无效"（对应 Memory 的 initValue = AllZero）
  val ents = RegInit(VecInit(Seq.fill(l.ageWords)(0.U(entW.W))))

  // =========================================================================
  // 扫描器：游标自增，发现 valid && !claim && 过期 即报告
  // =========================================================================
  val curB = RegInit(0.U(l.bankW.W))
  val curI = RegInit(0.U(l.idxW.W))
  val curW = RegInit(0.U(l.wayW.W))

  val curE = ents(flat(curB, curI, curW))
  // 未启用老化（ageW=0）时恒判不过期；不能拿 timeout=0 兜底，否则全表被删
  val curExp = if (ageW > 0) (io.now - eTs(curE)) >= timeout.U(l.ageW1.W) else false.B

  io.scanHit := io.scanEn && !io.scanHold && eValid(curE) && !eClaim(curE) && curExp
  io.scanBk  := curB
  io.scanIdx := curI
  io.scanWy  := curW

  val adv = io.scanEn && !io.scanHold
  when(adv) {
    when(curW === (l.ways - 1).U) {
      curW := 0.U
      when(curI === (l.bankDepth - 1).U) {
        curI := 0.U
        when(curB === (l.numBanks - 1).U) { curB := 0.U }.otherwise { curB := curB + 1.U }
      }.otherwise { curI := curI + 1.U }
    }.otherwise { curW := curW + 1.U }
  }

  // =========================================================================
  // 桶查询（组合读，供查找流水线 / 维护选路使用）
  // =========================================================================
  io.qEnt := VecInit((0 until l.numBanks).map { b =>
    VecInit((0 until l.ways).map { w =>
      ents(flat(b.U(l.bankW.W), io.qIdx(b), w.U(l.wayW.W)))
    })
  })

  // =========================================================================
  // 写口（单写口，字段级优先级 clr > clm > ins > rf）
  // =========================================================================
  val wrIdx = Wire(UInt(idxWid.W))
  val wrEnt = Wire(UInt(entW.W))
  val wrEn  = Wire(Bool())

  val insIdxE = flat(io.insBk, io.insIdx, io.insWy)
  val rfIdxE  = flat(io.rfBk, io.rfIdx, io.rfWy)
  val clrIdxE = flat(io.clrBk, io.clrIdx, io.clrWy)
  val clmIdxE = flat(io.clmBk, io.clmIdx, io.clmWy)

  wrEn  := io.clrEn || io.clmEn || io.insEn || io.rfEn
  wrIdx := Mux(io.clrEn, clrIdxE, Mux(io.clmEn, clmIdxE, Mux(io.insEn, insIdxE, rfIdxE)))
  wrEnt := Mux(io.clrEn, mkEnt(false.B, 0.U, false.B),
           Mux(io.clmEn, mkEnt(true.B, eTs(ents(clmIdxE)), true.B),
           Mux(io.insEn, mkEnt(true.B, io.now, false.B),
           /* rf */      mkEnt(true.B, io.now, eClaim(ents(rfIdxE))))))

  when(wrEn) { ents(wrIdx) := wrEnt }
}
