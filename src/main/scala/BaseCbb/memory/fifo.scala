package BaseCbb.memory

import chisel3._
import chisel3.util._
import BaseCbb.data.GenBundle
import BaseCbb.misc.ShiftRegInit

class fifo_wp [T<:Data] (gen:T) extends Bundle{
  val we    = Input(Bool())
  val wdata = Input(gen)
  val full  = Output(Bool())
}


class fifo_rp [T<:Data] (gen:T) extends GenBundle{
  val re    = Input(Bool())
  val rdata = Output(gen)
  val empty = Output(Bool())
}

/** 简易 FIFO（由 HBS `BaseCbb/memory/fifo.scala` 原样迁入，仅适配 HD 的 ShiftRegInit API）。
  *
  * NOTE(迁入时记录，未改动行为)：doEnq/doDeq 用的判据疑似写反 ——
  * `io.wp.full` / `io.rp.empty` 是 Output，读回来等于本地 `full` / `empty`，
  * 于是 doEnq = we && full、doDeq = re && empty，上电后永远不满/不空，数据通路不流动。
  * 保留原样以便与 HBS 行为对照，是否修正待 HBS 侧确认。
  */
class fifo [T<:Data] (gen:T,depth:Int,readLatency:Int) extends Module{
  val io = IO(new Bundle{
    val wp = new fifo_wp(gen)
    val rp = new fifo_rp(gen)
  })

  val enqPtr  = RegInit(0.U(log2Ceil(depth).W))
  val deqPtr  = RegInit(0.U(log2Ceil(depth).W))
  val Counter = RegInit(0.U(log2Ceil(depth+1).W)).suggestName("Counter")
  val ram     = Mem(depth,gen)

  val doEnq: Bool = io.wp.we && io.wp.full
  val doDeq: Bool = io.rp.re && io.rp.empty
  val full : Bool = RegInit(false.B)
  val empty : Bool = RegInit(false.B)

  val enqPtrInc: UInt = enqPtr + 1.U
  val deqPtrInc: UInt = deqPtr + 1.U
  when(doEnq && !doDeq){
    Counter := Counter + 1.U
  }.elsewhen(!doEnq && doDeq){
    Counter := Counter - 1.U
  }

  enqPtr := Mux(doEnq,enqPtrInc,enqPtr)
  deqPtr := Mux(doDeq,deqPtrInc,deqPtr)

  when(doEnq){
    ram(enqPtr) := io.wp.wdata
  }

  when(Counter===(depth-1).U && doEnq && !doDeq){
    full := true.B
  }.elsewhen(full && doDeq){
    full := false.B
  }

  when(Counter===1.U && !doEnq && doDeq){
    empty := true.B
  }.elsewhen(empty && doEnq){
    empty := false.B
  }

  io.rp.rdata := ShiftRegInit(ram(deqPtr),readLatency,0.U.asTypeOf(ram(deqPtr)),io.rp.re,Some("readdata"))
  io.rp.empty := empty
  io.wp.full  := full
}
