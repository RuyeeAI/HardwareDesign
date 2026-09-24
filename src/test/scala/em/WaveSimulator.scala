package em

import chisel3.simulator._

/**
 * 带波形 dump 的 Verilator 仿真器（Scala 侧驱动，不写 Verilog TB）。
 *
 * 为什么不能用 `EphemeralSimulator`：它拿不到 `Simulation.Controller`；
 * 而开波形需要两步，都只在 `Simulator` 的 `simulate` 里露出：
 *   ① 编译期 `TraceStyle(kind = TraceKind.Fst())` —— 给 Verilator 加 `--trace-fst`
 *      并定义 `SVSIM_ENABLE_FST_TRACING`
 *   ② 运行期 `dut.controller.setTraceEnabled(true)` —— 触发 DPI 回调里的 `$dumpfile` / `$dumpvars`
 *
 * 波形落盘路径由 svsim 固定：`<workspacePath>/workdir-<tag>/trace.fst`
 * （svsim 给仿真进程设 `SVSIM_SIMULATION_TRACE=<该绝对路径 stem>`，扩展名由
 * 编译期选择的 TraceKind 决定，与进程 cwd 无关）。
 *
 * chisel 7 起 svsim 的 Verilator 后端支持 FST（5.3/6.x 只有 VCD）；FST 无损且
 * 比 VCD 小约一个数量级，surfer / GTKWave 都能直接读。
 *
 * ⚠️ `simulate()` 返回的是 digest，**异常被吞在里面**。必须调 `.result` 才会把
 * 编译/仿真错误抛出来；否则编译失败会表现为"测试通过但没有波形"。
 *
 * ⚠️ chisel7 的 `simulate` body 只给 `SimulatedModule[T]`：用 `dut.wrapped`（即 ExactMatch）
 * 访问端口（peek/poke 经 `AnySimulatedModule.current` 解析到当前仿真），`dut.controller` 控制仿真。
 *
 * @param workspacePath 生成的 SV / obj_dir / 波形所在目录，相对当前工作目录。
 */
class WaveSimulator(override val workspacePath: String) extends Simulator[svsim.verilator.Backend] {
  val backend = svsim.verilator.Backend.initializeFromProcessEnvironment()
  val tag     = "verilator"
  val commonCompilationSettings = svsim.CommonCompilationSettings()
  val backendSpecificCompilationSettings =
    svsim.verilator.Backend.CompilationSettings(
      traceStyle = Some(svsim.verilator.Backend.CompilationSettings.TraceStyle(
        kind = svsim.verilator.Backend.CompilationSettings.TraceKind.Fst()))
    )

  /** 波形文件路径。 */
  def tracePath: String = s"$workspacePath/workdir-$tag/trace.fst"

  /** 与 [[BaseCbb.Sim]] 同理：chisel7 默认 `Randomization.random`（上电随机）会打破
    * 依赖"未复位寄存器为 0"的既有场景，这里固定为 uninitialized（chisel5 语义）。 */
  def simulateUninitialized[T <: chisel3.RawModule, U](module: => T)(body: SimulatedModule[T] => U) =
    simulate(module, settings = Settings.defaultRaw[T].copy(randomization = Randomization.uninitialized))(body)
}
