package gcd

import chisel3._

/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * Then our answer is in register x.
  */
class GCD extends Module {
  val io = IO(new Bundle {
    val a  = Input(UInt(16.W))
    val b  = Input(UInt(16.W))
    val e  = Input(Bool())
    val z  = Output(UInt(16.W))
    val v  = Output(Bool())
  })
  val x = Reg(UInt(16.W))
  val y = Reg(UInt(16.W))
  when (x > y && io.e) {
    x := x - y
  } .elsewhen (io.e) {
    x := y
    y := x
  }
  io.z := x
  io.v := y === 0.U
}
