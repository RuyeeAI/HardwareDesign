# tools/em_tb —— EM 波形审查

改动 RTL 后想看一眼波形，在 Scala 侧跑仿真出波形，再用 Surfer 打开。

```bash
sbt "testOnly em.EmWaveSpec"        # 跑 P1..P7 场景 + dump 波形 + 打印各阶段观测值
surfer -c tools/em_tb/em_wave.sucl out/em_wave/workdir-verilator/trace.fst
```

波形落在 `out/em_wave/workdir-verilator/trace.fst`（**FST 格式**，chisel 7 起 svsim 支持；
同场景 VCD ~351KB vs FST ~41KB，surfer/GTKWave 都能直接读。`out/` 在 .gitignore 内）。
场景、断言、每个阶段的观测值都打印在测试日志里，没查看器也能读。

## 文件

| 文件 | 说明 |
|------|------|
| `../../src/test/scala/em/EmWaveSpec.scala` | **波形场景本体**（P1..P7，Scala 侧驱动） |
| `../../src/test/scala/em/WaveSimulator.scala` | 带波形 dump 的 Verilator 仿真器封装 |
| `../../src/test/scala/em/EmTestSupport.scala` | 驱动 DUT 的公共脚手架（与 `ExactMatchSpec` 共用） |
| `em_wave.sucl` | Surfer 命令文件：预设信号分组 + `zoom_fit` |

## 场景的 7 个阶段（`EmWaveSpec`）

| 阶段 | 内容 | 期望观测 |
|------|------|----------|
| P1 | 复位 + 存储初始化握手 | `memInitDone=1`，`ktFree` 满值 |
| P2 | `add` ×2（第 2 次是覆盖写） | 首次插入 FSM 走 `S_HTREQ→S_HTW→S_HTD→S_DEC→S_ALLOC→S_WR`（**无 S_KTREQ**，指纹没命中省掉 KT 读）；覆盖写多出 `S_KTREQ→S_KTW`（指纹命中，读那 1 路 KT） |
| P3 | 命中查找 / 未插入查找 | `hit=1` / `hit=0` |
| P4 | **背靠背同 KEY**（需求 2） | 两个响应：先 `hit=0`（miss 并触发学习），再 `hit=1`（转发 CAM） |
| P5 | 连续灌命中流量（需求 1） | 40 拍稳态窗口内**每拍都有响应**、`key_ready` 不掉拍；停灌后流水线排空 |
| P6 | **老化**（需求 3） | `entries` 归 0 的同时 `ktFree`/`adFree` 回到满值 |
| P7 | 打满 HT + OVFC | `entries=70`、`ovfcUse=8`、`insFail=10` |

波形里 `sState` 的编码：`0=S_IDLE 2=S_HTREQ 3=S_HTW 4=S_KTREQ 5=S_KTW 6=S_DEC 7=S_ALLOC 8=S_WR 9=S_FREE 10=S_AGFR 11=S_HTD`。

## 波形开关：出不出波形

要开波形得**同时**满足两处（都写在 `WaveSimulator.scala` 里）：

| 位置 | 作用 |
|------|------|
| 编译期 `TraceStyle(kind = TraceKind.Fst())` | 给 Verilator 加 `--trace-fst` 并定义 `SVSIM_ENABLE_FST_TRACING` |
| 运行期 `dut.controller.setTraceEnabled(true)` | 触发 DPI 回调里的 `$dumpfile` / `$dumpvars` |

只做前一步 → 编出带 trace 的模型但一个字节都不落盘；只做后一步 → `$dumpvars` 被 `ifdef` 掉。
不想出波形就把 `traceStyle` 去掉 —— Verilator 不加 `--trace-fst` 时 `$dumpfile/$dumpvars` 被直接忽略
（0 报错、不落文件，仿真明显更快）。

dump 哪些信号由 svsim 生成的 `testbench.sv` 决定（`$dumpvars(0, dut)`，即 DUT 全层次
1100+ 个信号），Scala 侧改不了。

## ⚠️ 已知代价

**不能对波形做"输入与时钟沿同拍"类的自动检查。** svsim 通过 DPI poke 输入，
Verilator trace 给这些信号的时间戳是 eval 序号而非真实仿真时间，poke 的落点相对
时钟采样沿会偏移（实测：55 个请求在波形上按同拍判据只能数出 49 个，输出的响应拍
数也会差 2）。**看波形不受影响**（脉冲与响应都在、顺序也对），只是别用同拍判据去数。

   响应平衡的检查因此放在 Scala 断言里：`EmTestSupport.tick()` 每拍回调一次 `onCycle`，
   `EmWaveSpec` 用它统计"握手拍数 == 响应拍数"（`peek` 拿的是仿真器的真实值）。
   比以前 `vcd_check.py` 在波形上数更权威，也更早发现问题。

## 打开波形

```bash
surfer out/em_wave/workdir-verilator/trace.fst               # 直接打开
surfer -c tools/em_tb/em_wave.sucl out/em_wave/workdir-verilator/trace.fst   # 带预设视图
```

`em_wave.sucl` 已分好组（时钟握手 / 查找流水线 / svc引擎 / 计数与状态）并 `zoom_fit`。
在里面调好视图后可 `save_state_as xxx.surf.ron` 存下来，下次 `surfer -s xxx.surf.ron` 复用。

⚠️ `em_wave.sucl` 里的信号路径与仿真来源绑定：Scala 侧波形是
`TOP.svsimTestbench.<端口>` / `TOP.svsimTestbench.dut.<内部信号>`。

## 查看器

FST 是通用格式（GTKWave 系），换查看器不用重新仿真；需要 VCD 时用 `fst2vcd` 反向转换：

| 查看器 | 装法 | 说明 |
|---|---|---|
| **Surfer** | `brew install surfer` | 原生 arm64；读 FST / VCD，同一套代码也有 VS Code 扩展与浏览器版 |
| VaporView | VS Code 扩展 `lramseyer.vaporview` | 免费开源，读 VCD / FST / GHW |
| WaveTrace | VS Code 扩展 `wavetrace.wavetrace` | 免费限 8 个信号，**只读 VCD**（需先 `fst2vcd` 转换） |
| GTKWave | ⚠️ macOS 的 Homebrew cask 已停用（上游 discontinued） | 要走社区 tap 或源码编 GTKWave 4 |

## 注：为什么不用 iverilog

`AgeTable` 这类寄存器阵列（Vec）被 firtool 展成"连续赋值里对数组做变量下标读"，iverilog 要求常量
下标，编不过。仿真与波形都走 Verilator。
