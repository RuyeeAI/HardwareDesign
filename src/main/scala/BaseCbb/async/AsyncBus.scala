package BaseCbb.async

import chisel3.util._
import chisel3._

class AsyncBus[T<:Data](gen:T) extends Module{
  val io = IO(new Bundle {
    val clk_a   = Input(Clock())
    val rst_n_a = Input(Bool())
    val data_a  = Input(gen)
    val clk_b   = Input(Clock())
    val data_b  = Output(gen)
    val rst_n_b = Input(Bool())
  })
  val ack_b2a = Wire(Bool())
  val req_a2b = Wire(Bool())
  val a_bus_ff_q = Wire(gen)


  withClockAndReset(io.clk_a,!io.rst_n_a){
    val req  = RegNext(!ack_b2a,false.B)
    req_a2b := Sync(io.clk_b,req)
    val a_bus_ff = RegEnable(io.data_a,req === ack_b2a)
    a_bus_ff_q := a_bus_ff
  }

  withClockAndReset(io.clk_b,!io.rst_n_b){
    val ack = RegNext(req_a2b,false.B)
    ack_b2a := Sync(io.clk_a,ack)
    io.data_b := RegEnable(a_bus_ff_q,ack =/= req_a2b)
  }
}

