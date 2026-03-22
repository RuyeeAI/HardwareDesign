package BaseCbb.async
import chisel3._
import chisel3.util._
class AsyncPulse extends Module{
  val io = IO(new Bundle{
    val clk_a = Input(Clock())
    val rst_n_a = Input(Bool())
    val pulse_a = Input(Bool())
    val clk_b = Input(Clock())
    val rst_n_b = Input(Bool())
    val pulse_b = Output(Bool())
  })

  val pulse_a_ff = withClockAndReset(io.clk_a,!io.rst_n_a) {
    RegNext(io.pulse_a, false.B)
  }

  val pulse_a = io.pulse_a && !pulse_a_ff

  val req = withClockAndReset(io.clk_a,!io.rst_n_a){
    RegInit(false.B)
  }
  val req_a2b = Sync(io.clk_b,req).asBool
  val ack_b2a = Sync(io.clk_a,req_a2b).asBool

  val ack_ff  = withClockAndReset(io.clk_b,!io.rst_n_b){
    RegNext(req_a2b,false.B)
  }

  when(!req && pulse_a){
    req := true.B
  }.elsewhen(req && ack_b2a){
    req := false.B
  }

  io.pulse_b := !ack_ff && req_a2b
}
