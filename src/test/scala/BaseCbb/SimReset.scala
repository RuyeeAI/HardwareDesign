package BaseCbb

import chisel3._
import chisel3.simulator.EphemeralSimulator._

/** 显式复位助手。
  *
  * 背景：chiseltest 的默认后端是 treadle（进程内解释器），t=0 就给出 RegInit 值，
  * 所以老用例从不拉复位也能读到复位值。改用 chisel3.simulator（Verilator 后端）后，
  * 寄存器在复位前是 0，必须自己拉一次复位才能复现原有语义。
  *
  * 用法：`simulate(new Dut) { c => SimReset(c); ... }`
  */
object SimReset {
  private val AssertCycles = 2

  def apply(dut: Module): Unit = {
    dut.reset.poke(true.B)
    dut.clock.step(AssertCycles)
    dut.reset.poke(false.B)
  }
}
