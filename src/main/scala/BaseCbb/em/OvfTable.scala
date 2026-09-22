package em

import chisel3._
import chisel3.util._

// ===========================================================================
// OVFC —— HT 溢出表（寄存器阵列，全 key 并行比较，精确）
//
// 为什么需要它：插入时若"候选槽位里已有同指纹条目"（见 EmLayout 的插入规则）或 HT 已满，
//   条目进不了 HT。此时落到这里用**全 key** 精确比较兜住（不存在指纹误判），代价是只能
//   顺序扫描、容量小（ovfcDepth）。
//
// 自包含：条目寄存器（valid / claim / key / payload / ts）、并行匹配、空闲选择、
//   以及它自己的老化扫描游标都在这里。对外的读写口都按"条目序号"寻址。
//
// claim 位与 HT 侧（AgeTable）同义：扫描器认定某条过期 → 先置 claim（**不清 valid**，
//   查找仍可命中它）→ 走向 svc 的"老化动作"任务 → 同拍清 valid + claim。维护插入的
//   空闲选择必须把 claim 当占用。
//
// 写优先级：clm < wr（同拍都指向同一条时，wr 的清 claim 生效 —— 即"刚写进去的条目
//   不该被老化掉"，与 HT 侧 clr > clm 的用意一致）。
// ===========================================================================
class OvfTable(l: EmLayout, timeout: BigInt) extends Module {
  private val en   = l.ovfcEn                          // ovfcDepth > 0 才启用；false 时全部输出为 0
  private val d    = l.p.ovfcDepth
  private val selW = l.ovfcCntW                         // 序号位宽（与 ExactMatch 侧寄存器一致）
  private val payW = math.max(1, l.ovfcPayW)
  private val tsW  = math.max(1, l.ageW1)

  val io = IO(new Bundle {
    val now = Input(UInt(tsW.W))

    // ---- 并行匹配：两个独立查询口 ----
    // 为什么要两个：查找接收拍（用 io.key.bits）与 svc 任务启动拍（用 wr/学习 head 的 key）
    // 可能**同拍**发生，共用一个口会互相踩。
    val lkKey = Input(UInt(l.keyW.W)); val lkHit = Output(Bool()); val lkSel = Output(UInt(selW.W)); val lkPay = Output(UInt(payW.W))
    val svKey = Input(UInt(l.keyW.W)); val svHit = Output(Bool()); val svSel = Output(UInt(selW.W)); val svPay = Output(UInt(payW.W))

    // ---- 分配：找一条空闲（!valid 且 !claim）----
    val hasFree = Output(Bool())
    val allocSel = Output(UInt(selW.W))

    // ---- 老化扫描：游标自增，发现"valid && !claim && 过期"即报命中 ----
    val scanEn    = Input(Bool())      // 已含 ageEn / sweepEnable / memReady 门控
    val scanHold  = Input(Bool())      // 已有 pending 老化任务 → 暂停推进
    val scanBlock = Input(Bool())      // 有作废在跑 → 本拍不报命中（游标照常前进）
    val scanHit   = Output(Bool())
    val scanSel   = Output(UInt(selW.W))

    // ---- 写口 ----
    val clmEn  = Input(Bool()); val clmSel  = Input(UInt(selW.W))   // 置 claim（扫描器抢到目标）
    val wrEn   = Input(Bool()); val wrSel   = Input(UInt(selW.W))   // 写入/覆盖：valid=1, claim=0, ts=now
    val wrKey  = Input(UInt(l.keyW.W)); val wrPay = Input(UInt(payW.W)); val wrCountUp = Input(Bool())
    val rfEn   = Input(Bool()); val rfSel   = Input(UInt(selW.W))   // 命中刷新：只写 ts=now
    val freeEn = Input(Bool()); val freeSel = Input(UInt(selW.W))   // 释放（删除 / 老化完成 / UE 放弃）

    // ---- 按序号查询 ----
    val agSel    = Input(UInt(selW.W))                     // 老化 pending 项序号
    val agPay    = Output(UInt(payW.W))                    // 该项 payload
    val agClaim  = Output(Bool())                          // 该项 claim（判断"claim 还在不在"）
    val svPaySel = Input(UInt(selW.W))                     // 维护任务路径按序号取 payload
    val svPayQ   = Output(UInt(payW.W))

    val count = Output(UInt(selW.W))                       // 当前占用条数
  })

  private val v    = if (en) RegInit(VecInit(Seq.fill(d)(false.B))) else null
  private val c    = if (en) RegInit(VecInit(Seq.fill(d)(false.B))) else null
  private val k    = if (en) Reg(Vec(d, UInt(l.keyW.W))) else null
  private val p    = if (en) Reg(Vec(d, UInt(payW.W))) else null
  private val t    = if (en) Reg(Vec(d, UInt(tsW.W))) else null
  private val use  = if (en) RegInit(0.U(selW.W)) else null

  /** 全 key 并行匹配（序号小的优先）；未启用时恒 miss */
  private def matchOf(key: UInt): (Bool, UInt) =
    if (!en) (false.B, 0.U(selW.W))
    else {
      val hit = VecInit((0 until d).map(i => v(i) && !c(i) && k(i) === key))
      (hit.asUInt.orR, PriorityEncoder(hit))
    }

  private def expired(ts: UInt): Bool =
    if (l.agingOn) (io.now - ts) >= timeout.U(tsW.W) else false.B

  val (lkH, lkS) = matchOf(io.lkKey)
  val (svH, svS) = matchOf(io.svKey)
  io.lkHit := lkH; io.lkSel := lkS; io.lkPay := (if (en) p(lkS) else 0.U(payW.W))
  io.svHit := svH; io.svSel := svS; io.svPay := (if (en) p(svS) else 0.U(payW.W))

  io.hasFree := (if (en) use =/= d.U else false.B)
  io.allocSel := (if (en) PriorityEncoder(VecInit((0 until d).map(i => !v(i) && !c(i)))) else 0.U(selW.W))

  io.agPay   := (if (en) p(io.agSel) else 0.U(payW.W))
  io.agClaim := (if (en) c(io.agSel) else false.B)
  io.svPayQ  := (if (en) p(io.svPaySel) else 0.U(payW.W))
  io.count   := (if (en) use else 0.U(selW.W))

  // =========================================================================
  // 扫描器：游标自增（scanEn 且没有待处理的老化任务）
  // =========================================================================
  private val scIdx = if (en) RegInit(0.U(selW.W)) else null
  private val scanAdv = io.scanEn && !io.scanHold
  private val cand = if (en) v(scIdx) && !c(scIdx) && expired(t(scIdx)) else false.B

  io.scanHit := (if (en) scanAdv && !io.scanBlock && cand else false.B)
  io.scanSel := (if (en) scIdx else 0.U(selW.W))
  if (en) {
    when(scanAdv) { scIdx := Mux(scIdx === (d - 1).U, 0.U, scIdx + 1.U) }
  }

  // =========================================================================
  // 写口
  // =========================================================================
  if (en) {
    // ① 置 claim —— 放在 wr 之前，让下面的"写入清 claim"能覆盖它
    when(io.clmEn) { c(io.clmSel) := true.B }

    // ② 写入/覆盖（新增时才计数+1）
    when(io.wrEn) {
      v(io.wrSel) := true.B
      c(io.wrSel) := false.B
      k(io.wrSel) := io.wrKey
      p(io.wrSel) := io.wrPay
      t(io.wrSel) := io.now
      when(io.wrCountUp) { use := use + 1.U }
    }

    // ③ 命中刷新：只写 ts（不动 valid/claim/计数 —— 不可能复活已老化条目）
    when(io.rfEn) { t(io.rfSel) := io.now }

    // ④ 释放：清 valid/claim 并计数减（三条来源：删除、老化完成、UE 放弃）
    when(io.freeEn) {
      v(io.freeSel) := false.B
      c(io.freeSel) := false.B
      when(use =/= 0.U) { use := use - 1.U }
    }
  }
}