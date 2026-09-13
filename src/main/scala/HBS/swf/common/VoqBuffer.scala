package HBS.swf.common

import BaseCbb.data.GenModule
import BaseCbb.memory.{Memory, MemoryAccessType, _}
import chisel3._
import chisel3.util._

class VoqBuffer(BufferDep: Int, DataW: Int, QueueNum: Int) extends GenModule {
  val clk = IO(Input(Clock()))
  val rst_n = IO(Input(Bool()))
  val io = IO(new Bundle {
    val i_enq_data  = Input(UInt(DataW.W))
    val i_enq_valid = Input(Bool())
    val i_enq_qid   = Input(UInt(log2Ceil(QueueNum).W))
    val i_deq_valid = Input(Vec(QueueNum, Bool()))
    val o_deq_data  = Output(Valid(UInt(DataW.W)))
  })

  withClockAndReset(clk,rst_n) {
    // 原 HBS 写法 Memory(..., "1R1W")；HD 的 Memory 用 MemoryAccessType 枚举（TP = 1R1W）。
    // flopIn/flopOut 显式给出，保持 HBS 的默认插拍语义（HD 的 flopIn 默认是 false）。
    val U_DATA_MEM = Module(new TpMemoryWrap(Memory("DATA", UInt(DataW.W), BufferDep,
      memoryType = MemoryAccessType.TP, flopIn = true, flopOut = true)))
    val U_BITMAP   = Module(new Bitmap(RscNum = BufferDep))
    val U_VOQ      = Module(new VoqLinkList(RscNum = BufferDep,QueueNum = QueueNum,RamLat = 4,PtrW = log2Ceil(BufferDep)))
    val U_LL_MEM   = Module(new TpMemoryWrap(Memory("LL",UInt(log2Ceil(BufferDep).W),depth=BufferDep,
      memoryType = MemoryAccessType.TP, flopIn = true, flopOut = true)))
    U_BITMAP.io.req_vld := io.i_enq_valid
    U_BITMAP.io.ret_vld := io.i_deq_valid.reduceTree(_|_)
    U_BITMAP.io.ret_ptr := U_VOQ.io.o_deq_ptr

    U_DATA_MEM.clk       := clk
    U_DATA_MEM.rst_n     := rst_n
    U_DATA_MEM.lgc.we    := io.i_enq_valid
    U_DATA_MEM.lgc.wdata := io.i_enq_data
    U_DATA_MEM.lgc.waddr := U_BITMAP.io.req_ptr
    U_DATA_MEM.lgc.re    := io.i_deq_valid.reduceTree(_|_)
    U_DATA_MEM.lgc.raddr := U_VOQ.io.o_deq_ptr
    io.o_deq_data.bits   := U_DATA_MEM.lgc.rdata
    io.o_deq_data.valid  := ShiftRegister(io.i_deq_valid.reduceTree(_|_),4)
    U_VOQ.io.i_enq     := io.i_enq_valid
    U_VOQ.io.i_enq_ptr := U_BITMAP.io.req_ptr
    U_VOQ.io.i_enq_qid := io.i_enq_qid
    U_VOQ.io.i_deq     := io.i_deq_valid

    U_LL_MEM.lgc <> U_VOQ.io.ll_mem_intf
    U_LL_MEM.clk := clk
    U_LL_MEM.rst_n := rst_n
  }
}
