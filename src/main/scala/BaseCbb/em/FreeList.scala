package em

import chisel3._
import chisel3.util._

// ===========================================================================
// 空闲条目池（free list）—— bitmap + 优先编码器
//
// 位图语义：`bmp(i) = 1` 表示条目 i 空闲，初始全空闲（对应 Memory 的 initValue=AllZero）。
//   alloc：取**编号最低**的空闲位，并把它清零
//   free ：把 faddr 对应的位置 1
//   count：位图的 popcount
//
// 为什么不用"寄存器栈 + sp"：depth 大时（KT 默认 4096 / AD 1024）栈要 depth×addrW 个 flop
//   （4096×12 ≈ 49k flop），换成位图只要 depth 个 flop + 一棵优先编码树，面积降一个数量级。
//   代价是分配顺序从 LIFO 变成"最低编号优先"——对外语义等价（任何空闲条目都可分配）。
//
// 同拍 alloc + free：净数量不变，结果 = (位图清掉 alloc 位) | (置上 free 位)。
//   与旧实现一致（旧实现是把归还的地址塞进刚挖出的坑）。二者目标同址的极端情况两边
//   都是"该地址既被分配又仍空闲"，但上层（ExactMatch 的 S_ALLOC 与 S_FREE/S_AGFR）
//   互斥，不会同拍发生。
// ===========================================================================
class FreeList(depth: Int) extends Module {
  require(depth >= 1, "FreeList depth 必须 >= 1")
  val addrW = math.max(1, log2Ceil(depth))
  val cntW  = log2Ceil(depth + 1)

  val io = IO(new Bundle {
    val alloc = Input(Bool())            // 请求分配
    val addr  = Output(UInt(addrW.W))    // 分配到的地址（alloc 且 ok 时有效）
    val ok    = Output(Bool())           // 有空闲条目
    val free  = Input(Bool())            // 归还
    val faddr = Input(UInt(addrW.W))     // 归还的地址
    val count = Output(UInt(cntW.W))     // 当前空闲条目数
  })

  val bmp    = RegInit(VecInit(Seq.fill(depth)(true.B)))
  val freeVec = bmp.asUInt

  val okNow    = freeVec.orR
  val allocIdx = PriorityEncoder(freeVec)      // 最低位的 1 → 最低编号的空闲条目

  io.ok    := okNow
  io.addr  := allocIdx
  io.count := PopCount(freeVec)

  val doAlloc   = io.alloc && okNow
  val allocMask = Mux(doAlloc, UIntToOH(allocIdx, depth), 0.U(depth.W))
  val freeMask  = Mux(io.free, UIntToOH(io.faddr, depth), 0.U(depth.W))

  when(doAlloc || io.free) {
    bmp := ((freeVec & ~allocMask) | freeMask).asTypeOf(Vec(depth, Bool()))
  }
}
