package BaseCbb

import chisel3.RawModule
import chisel3.simulator.{LayerControl, PeekPokeAPI, Randomization, Settings}
import chisel3.testing.HasTestingDirectory

/** chisel5 行为兼容的 ephemeral 仿真入口（用法与 `chisel3.simulator.EphemeralSimulator` 完全一致）。
  *
  * 背景：chisel 7 的 ChiselSim 把默认随机化改成了 `Randomization.random`
  * （所有未初始化寄存器/存储上电为**随机值**），而 chisel 5.3 时代 Verilator（two-state）
  * 未初始化即 **0**。本仓库大量既有用例依赖"不拉复位、t=0 读到 0"的旧语义，直接用
  * `EphemeralSimulator` 会大面积假失败（读出垃圾、initDone 随机置 1 等）。
  *
  * 这里固定 `Randomization.uninitialized` 恢复旧语义；新写用例若想验证复位完整性，
  * 可显式用 `chisel3.simulator.EphemeralSimulator`（随机初值能抓到漏复位的寄存器）。
  */
object Sim extends PeekPokeAPI {

  private val chiselSim = new chisel3.simulator.ChiselSim {}

  def simulate[T <: RawModule](
    module:       => T,
    layerControl: LayerControl.Type = LayerControl.EnableAll
  )(body: (T) => Unit): Unit = {
    implicit val temporary: HasTestingDirectory = HasTestingDirectory.temporary(deleteOnExit = true)
    chiselSim.simulateRaw(
      module,
      settings = Settings.defaultRaw[T].copy(
        verilogLayers = layerControl,
        randomization = Randomization.uninitialized
      )
    )(body)
  }
}
