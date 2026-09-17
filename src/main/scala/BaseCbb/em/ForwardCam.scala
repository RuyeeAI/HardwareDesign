package em

import chisel3._
import chisel3.util._

// ===========================================================================
// 在途插入转发 CAM（read-after-write forwarding for self-learning）
//
// 解决的需求：**背靠背两个同 KEY 请求，第 1 个 Miss，第 2 个要能 Hit。**
//
// 为什么必须有它：
//   自学习插入（分配 KT/AD 指针 → 写 HT）本身要十几拍，而流水线版查找的
//   接受间隔只有 1 拍。第 2 个请求进入流水线时，第 1 个请求触发的插入
//   还在半路，HT 里根本没有该条目 → 按裸表查一定 Miss。
//   所以"第 1 个 Miss 之后"必须在**判定 Miss 的那一拍**就把 (key, AD) 记下来，
//   后续查找在桶比较之前先比这张 CAM，命中即当作 Hit 直接返回 AD。
//
// 时序：push 发生在流水线判定 Miss 的当拍（同拍可见后续请求）；
//       pop 发生在维护引擎完成该条目插入的当拍（此后表里已能查到，转发不再需要）。
//
// 重复 key：同 key 可能有多条在途（连续 Miss），比对时取**最新**一条
//   （L2 学习的语义是"最后一次学到的入端口生效"）。
//
// 反压：CAM 满时上层拉低 io.key.ready，避免漏学。
// ===========================================================================

class ForwardCam(depth: Int, keyW: Int, adW: Int) extends Module {
  require(depth >= 2, "ForwardCam depth 必须 >= 2")
  private val ptrW = math.max(1, log2Ceil(depth))
  private val cntW = math.max(1, log2Ceil(depth + 1))

  val io = IO(new Bundle {
    val push    = Input(Bool())
    val pushKey = Input(UInt(keyW.W))
    val pushAd  = Input(UInt(adW.W))
    val pop     = Input(Bool())                     // 最老一条已完成插入
    val lookKey = Input(UInt(keyW.W))               // 流水线待比对的 key
    val headKey = Output(UInt(keyW.W))              // 最老一条（维护引擎取它做插入）
    val headAd  = Output(UInt(adW.W))
    val hit     = Output(Bool())
    val hitAd   = Output(UInt(adW.W))
    val full    = Output(Bool())
    val empty   = Output(Bool())
    val count   = Output(UInt(cntW.W))
  })

  val v    = RegInit(VecInit(Seq.fill(depth)(false.B)))
  val key  = Reg(Vec(depth, UInt(keyW.W)))
  val ad   = Reg(Vec(depth, UInt(adW.W)))
  val head = RegInit(0.U(ptrW.W))
  val tail = RegInit(0.U(ptrW.W))
  val occ  = RegInit(0.U(cntW.W))

  io.count := occ
  io.full  := occ === depth.U
  io.empty := occ === 0.U

  /** 偏移 o（0 = 最老）对应的物理槽位 */
  private def idxOf(o: UInt): UInt = {
    val s = head +& o
    Mux(s >= depth.U, s - depth.U, s)(ptrW - 1, 0)
  }

  // ---- 比对：按"年龄偏移"建匹配向量，取偏移最大（最新）的一条 ----
  val matches = VecInit((0 until depth).map { o =>
    val i = idxOf(o.U(ptrW.W))
    v(i) && key(i) === io.lookKey
  })
  val selRev = PriorityEncoder(matches.reverse)      // reverse 后优先级最低位 = 偏移最大
  val selO   = (depth - 1).U - selRev
  io.hit   := matches.asUInt.orR
  io.hitAd := VecInit((0 until depth).map(o => ad(idxOf(o.U(ptrW.W)))))(selO)

  io.headKey := key(idxOf(0.U(ptrW.W)))
  io.headAd  := ad(idxOf(0.U(ptrW.W)))

  // ---- 入队 / 出队 ----
  val doPop  = io.pop && !io.empty
  val doPush = io.push && (!io.full || doPop)        // 同拍出队时允许补位

  when(doPop) {
    v(head) := false.B
    head := Mux(head === (depth - 1).U, 0.U, head + 1.U)
  }
  when(doPush) {
    v(tail)   := true.B
    key(tail) := io.pushKey
    ad(tail)  := io.pushAd
    tail := Mux(tail === (depth - 1).U, 0.U, tail + 1.U)
  }
  occ := occ + doPush.asUInt - doPop.asUInt
}
