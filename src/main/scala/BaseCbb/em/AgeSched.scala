package em

import chisel3._
import chisel3.util._

// ===========================================================================
// 老化调度器 —— "同时只处理一个可老化目标" 的 claim / 优先级胶水
//
// 两路扫描器各自报"游标处有过期条目"：
//   · HT 槽位（AgeTable 内部扫描器）
//   · OVFC 项（OvfTable 内部游标）
// 本模块把它们仲裁成**一个 pending 目标**（HT 优先），并：
//   · 在选中的同拍给对应条目置 claim（此后扫描器不会再重复报它，插入把它当占用）
//   · 一直保持该目标，直到下游（svc 任务完成 / 放弃）用 release 放掉
//
// ⚠️ claim 必须与"记录目标"同拍、且共用同一套优先级：
//   HT 命中时只能 claim HT，OVFC 那一路**必须不置 claim**。否则 OVFC 项会被 claim 住却
//   没记下序号 —— 该项既不会被扫描器再次发现（scanHit 要求 !claim），插入又把它当占用，
//   于是永久不可用（OVFC 容量泄漏）。这就是这里把 claim 使能与仲裁写在同一个模块的原因。
//
// release 与 claim 同拍：按下面的写法 claim 优先（pend 保持为真、目标不丢）。
//   实际不可能同拍 —— 所有 release 的来源都要求 pend 已经是 1（老化任务在途），
//   而 claim 要求 pend=0。取 claim 优先只是"万一撞上也不泄漏"的保险。
// ===========================================================================
class AgeSched(l: EmLayout) extends Module {
  /** 序号位宽：与 ExactMatch / OvfTable 侧一致（含 ovfcDepth=0 的退化情形） */
  val selW = l.ovfcCntW

  val io = IO(new Bundle {
    // ---- 上游：扫描使能 / 有 UE 作废在跑 ----
    val scanEn  = Input(Bool())        // 已含 ageEn / sweepEnable / memReady 门控
    val invBusy = Input(Bool())

    // ---- 两路扫描结果 ----
    val htHit = Input(Bool())
    val htBk  = Input(UInt(l.bankW.W)); val htIdx = Input(UInt(l.idxW.W)); val htWy = Input(UInt(l.wayW.W))
    val ovHit = Input(Bool()); val ovSel = Input(UInt(selW.W))

    // ---- 下游释放（老化完成 / UE 放弃 / 发现 claim 已丢）----
    val release = Input(Bool())

    // ---- claim 使能（地址由两个表用自己的扫描游标给出）----
    val htClmEn = Output(Bool())
    val ovClmEn = Output(Bool())

    // ---- pending 目标 ----
    val pend      = Output(Bool())
    val pendOv    = Output(Bool())
    val pendBk    = Output(UInt(l.bankW.W)); val pendIdx = Output(UInt(l.idxW.W)); val pendWy = Output(UInt(l.wayW.W))
    val pendSlot  = Output(UInt(l.slotW.W))                 // 扁平槽位 b*ways+w（索引 sPay/sClm）
    val pendOvSel = Output(UInt(selW.W))
  })

  val pend   = RegInit(false.B)
  val pendOv = RegInit(false.B)
  val bk  = Reg(UInt(l.bankW.W))
  val idx = Reg(UInt(l.idxW.W))
  val wy  = Reg(UInt(l.wayW.W))
  val ovSelR = Reg(UInt(selW.W))

  val free  = !pend && !io.invBusy
  val htWin = io.scanEn && free && io.htHit
  val ovWin = io.scanEn && free && !io.htHit && io.ovHit

  io.htClmEn := htWin
  io.ovClmEn := ovWin

  when(htWin) {
    pend := true.B; pendOv := false.B
    bk := io.htBk; idx := io.htIdx; wy := io.htWy
  }.elsewhen(ovWin) {
    pend := true.B; pendOv := true.B
    ovSelR := io.ovSel
  }.elsewhen(io.release) {
    pend := false.B
  }

  io.pend      := pend
  io.pendOv    := pendOv
  io.pendBk    := bk
  io.pendIdx   := idx
  io.pendWy    := wy
  io.pendSlot  := (bk * l.ways.U + wy)(l.slotW - 1, 0)
  io.pendOvSel := ovSelR
}