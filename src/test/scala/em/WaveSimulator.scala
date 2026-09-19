package em

import chisel3.simulator._

/**
 * 带波形 dump 的 Verilator 仿真器（Scala 侧驱动，不写 Verilog TB）。
 *
 * 为什么不能用 `EphemeralSimulator`：它是 2 参数 `simulate(gen)(dut => ...)`，拿不到 `Controller`；
 * 而开波形需要两步，都只在 `SingleBackendSimulator` 的 3 参数 `simulate` 里露出：
 *   ① 编译期 `Backend.CompilationSettings(traceStyle = Some(TraceStyle.Vcd()))` —— 给 Verilator 加 `--trace`
 *      并定义 `SVSIM_ENABLE_VCD_TRACING`
 *   ② 运行期 `controller.setTraceEnabled(true)` —— 触发 DPI 回调里的 `$dumpfile` / `$dumpvars`
 *
 * 波形落盘路径由 svsim 固定：`<workspacePath>/workdir-<tag>/trace.vcd`
 * （svsim 会给仿真进程设 `SVSIM_SIMULATION_TRACE=<该绝对路径>/trace`，与进程 cwd 无关）。
 * 对本类即 `out/em_wave/workdir-verilator/trace.vcd`。
 *
 * ⚠️ svsim 的 Verilator 后端只提供 `TraceStyle.Vcd`，没有 FST。
 *
 * ⚠️ `simulate()` 返回的是 digest，**异常被吞在里面**。必须调 `.result` 才会把
 * 编译/仿真错误抛出来；否则编译失败会表现为"测试通过但没有波形"。
 *
 * @param workspacePath 生成的 SV / obj_dir / 波形所在目录，相对当前工作目录。
 */
class WaveSimulator(override val workspacePath: String) extends SingleBackendSimulator[svsim.verilator.Backend] {
  val backend = svsim.verilator.Backend.initializeFromProcessEnvironment()
  val tag     = "verilator"
  val commonCompilationSettings = svsim.CommonCompilationSettings()
  val backendSpecificCompilationSettings =
    svsim.verilator.Backend.CompilationSettings(
      traceStyle = Some(svsim.verilator.Backend.CompilationSettings.TraceStyle.Vcd())
    )

  /** 波形文件路径。 */
  def tracePath: String = s"$workspacePath/workdir-$tag/trace.vcd"
}
