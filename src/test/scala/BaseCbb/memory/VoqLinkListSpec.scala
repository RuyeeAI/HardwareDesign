package BaseCbb.memory

import chisel3._
import BaseCbb.Sim._
import chisel3.util.{log2Ceil, ShiftRegister}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import BaseCbb.SimReset

/** VoqLinkList 功能仿真（2026-09-13 索引修复的回归锁定）。
  *
  * 修复前：enq 路由缺 qid 匹配、deq/deqSel/enqSel 索引混排、计数器模数误用 QueueNum
  * —— 仅 QueueNum==RamLat 时碰巧正确。本测试特意选 QueueNum=2 / RamLat=4（不相等）。
  *
  * 每队列把第 n 次访问轮转到 lane=n%RamLat：连续出队天然分散到不同子链表，
  * 每个子链表有 RamLat 拍完成读回，因此逐拍连续出队无需等待。
  * 注意 o_deq_ptr 是出队当拍的组合输出（head_ptr_mux），须在该拍内采样。
  */
class VoqLinkListSpec extends AnyFreeSpec with Matchers {

  /** 外部 TP SRAM 模型：读延迟恰为 ramLat 拍（与 SubLinklist 内 ShiftRegister(re, RamLat) 匹配）。
    * TpMemoryPort 本身就是存储器视角（rdata 为 Output），模型直接例化、不再 Flipped。 */
  class TpSramModel(depth: Int, ramLat: Int, dataW: Int, addrW: Int) extends Module {
    val io = IO(new TpMemoryPort(addrW, dataW))
    val mem = SyncReadMem(depth, UInt(dataW.W))
    val rdataPipe = ShiftRegister(mem.read(io.raddr, io.re), ramLat - 1, 0.U, true.B)
    io.rdata := rdataPipe
    when(io.we) { mem.write(io.waddr, io.wdata) }
  }

  /** VoqLinkList + 外部 SRAM 的完整封装（DUT 的 ll_mem_intf 在本层闭合）。 */
  class VoqLinkListWrapper(q: Int, lat: Int, rsc: Int, pw: Int) extends Module {
    val io = IO(new Bundle {
      val i_enq      = Input(Bool())
      val i_enq_ptr  = Input(UInt(pw.W))
      val i_enq_qid  = Input(UInt(log2Ceil(q).W))
      val i_deq      = Input(Vec(q, Bool()))
      val o_deq_ptr  = Output(UInt(pw.W))
      val o_empty    = Output(Bool())
      val o_full     = Output(Bool())
      val o_head_ptr = Output(UInt(pw.W))
      val o_tail_ptr = Output(UInt(pw.W))
    })
    val dut  = Module(new VoqLinkList(q, lat, rsc, pw))
    val sram = Module(new TpSramModel(rsc, lat, pw, log2Ceil(rsc)))
    dut.io.ll_mem_intf <> sram.io
    dut.io.i_enq     := io.i_enq
    dut.io.i_enq_ptr := io.i_enq_ptr
    dut.io.i_enq_qid := io.i_enq_qid
    dut.io.i_deq     := io.i_deq
    io.o_deq_ptr  := dut.io.o_deq_ptr
    io.o_empty    := dut.io.o_empty
    io.o_full     := dut.io.o_full
    io.o_head_ptr := dut.io.o_head_ptr
    io.o_tail_ptr := dut.io.o_tail_ptr
  }

  private val Q = 2
  private val L = 4 // 特意与 QueueNum 不相等（修复前仅在相等时碰巧正确）
  private val R = 16
  private val W = 4

  "多队列链表：按队列 FIFO 顺序出队，QueueNum(2) != RamLat(4)" in {
    simulate(new VoqLinkListWrapper(Q, L, R, W)) { c =>
      SimReset(c)
      def enq(q: Int, ptr: Int): Unit = {
        c.io.i_enq.poke(true.B)
        c.io.i_enq_ptr.poke(ptr.U)
        c.io.i_enq_qid.poke(q.U)
        c.clock.step(1)
        c.io.i_enq.poke(false.B)
      }
      def deq(q: Int, expect: Int): Unit = {
        for (j <- 0 until Q) c.io.i_deq(j).poke((j == q).B)
        // o_deq_ptr 是出队当拍的组合输出，必须在该拍内采样
        val got = c.io.o_deq_ptr.peek().litValue.toInt
        c.clock.step(1)
        for (j <- 0 until Q) c.io.i_deq(j).poke(false.B)
        assert(got == expect, s"队列 $q 出队指针期望 $expect 实际 $got")
      }

      // 入队：q0 = 1,2,3,4；q1 = 9,10,11,12（逐拍，避免共享单口冲突）
      enq(0, 1)
      enq(1, 9)
      enq(0, 2)
      enq(1, 10)
      enq(0, 3)
      enq(1, 11)
      enq(0, 4)
      enq(1, 12)

      // 出队：各队列按 FIFO 顺序；lane 随 seq 轮转（0→1→2→3）
      deq(0, 1)
      deq(0, 2)
      deq(0, 3)
      deq(0, 4)
      // 此时 q1 的 4 项仍在队（o_empty 是全部子链表的与，应为非空）

      deq(1, 9)
      deq(1, 10)
      deq(1, 11)
      deq(1, 12)
      assert(c.io.o_empty.peek().litValue != 0, "两队列各出队 4 个后链表应空")
    }
  }

  "两队列交替出队（每拍一个事务，lane 互不冲突）" in {
    simulate(new VoqLinkListWrapper(Q, L, R, W)) { c =>
      SimReset(c)
      def enq(q: Int, ptr: Int): Unit = {
        c.io.i_enq.poke(true.B)
        c.io.i_enq_ptr.poke(ptr.U)
        c.io.i_enq_qid.poke(q.U)
        c.clock.step(1)
        c.io.i_enq.poke(false.B)
      }
      def deq(q: Int, expect: Int): Unit = {
        for (j <- 0 until Q) c.io.i_deq(j).poke((j == q).B)
        val got = c.io.o_deq_ptr.peek().litValue.toInt
        c.clock.step(1)
        for (j <- 0 until Q) c.io.i_deq(j).poke(false.B)
        assert(got == expect)
      }

      enq(0, 5)
      enq(0, 6)
      enq(1, 13)
      enq(0, 7)
      enq(1, 14)

      // 两队列交替出队：各队列独立 lane 轮转，顺序互不干扰
      deq(0, 5)
      deq(1, 13)
      deq(0, 6)
      deq(1, 14)
      deq(0, 7)
      assert(c.io.o_empty.peek().litValue != 0)
    }
  }
}
