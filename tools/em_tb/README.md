# tools/em_tb —— EM 波形审查

改动 RTL 后想看一眼波形，用这里的脚本一键出图。

```bash
tools/em_tb/run_wave.sh              # 出 FST 并直接起 Surfer（默认）
tools/em_tb/run_wave.sh --vcd        # 出 VCD（可用 vcd_check.py 自动检查）
tools/em_tb/run_wave.sh --no-open    # 只跑仿真，不出图形界面（CI/脚本）
```

流程：`em.EmGen` 产出 Verilog → Verilator 编译 → 跑 testbench → 出波形 →（可选）起 Surfer
并自动加载预设视图。产物都在 `out/em_tb/`（.gitignore 内）。

依赖：`sbt`、`verilator`（`brew install verilator`）、`surfer`（`brew install surfer`）。
**不要用 iverilog**：`AgeTable` / `FreeList` 被 firtool 展成"连续赋值里对数组做变量下标读"，
iverilog 要求常量下标，编不过。

## 文件

| 文件 | 说明 |
|------|------|
| `run_wave.sh` | 一键流程（生成 → 编译 → 仿真 → 起 Surfer） |
| `tb_exact_match.v` | testbench，7 个场景阶段；与 `preset=tb` 绑定 |
| `em_wave.sucl` | Surfer 命令文件：预设要看的信号分组 + `zoom_fit` |
| `vcd_check.py` | VCD 自动检查（响应平衡等），只支持 VCD |

## testbench 的 7 个阶段

| 阶段 | 内容 | 期望观测 |
|------|------|----------|
| P1 | 复位 + 存储初始化握手 | `memInitDone=1`，`ktFree` 满值 |
| P2 | `add` ×2（第 2 次是覆盖写） | 首次插入 FSM 走 `S_HTREQ→S_HTW→S_HTD→S_DEC→S_ALLOC→S_WR`（**无 S_KTREQ**，指纹没命中省掉 KT 读）；覆盖写多出 `S_KTREQ→S_KTW`（指纹命中，读那 1 路 KT） |
| P3 | 命中查找 / 未插入查找 | `hit=1` / `hit=0` |
| P4 | **背靠背同 KEY**（需求 2） | 两个响应：先 `hit=0`（miss 并触发学习），再 `hit=1`（转发 CAM） |
| P5 | 连续灌命中流量（需求 1） | `rsp_valid` 每拍都高（无 svc 活儿时 40 拍 40 个响应） |
| P6 | **老化**（需求 3） | `entries` 归 0 的同一拍 `ktFree`/`adFree` 回满 |
| P7 | 打满 HT + OVFC | `entries=70`、`ovfcUse=8`、`insFail=10` |

波形里 `sState` 的编码：`0=S_IDLE 2=S_HTREQ 3=S_HTW 4=S_KTREQ 5=S_KTW 6=S_DEC 7=S_ALLOC 8=S_WR 9=S_FREE 10=S_AGFR 11=S_HTD`。

## 波形开关：出不出波形

**开关在 Verilator 编译那一步，TB 不用改** —— Verilator 不加 `--trace*` 时，TB 里的
`$dumpfile/$dumpvars` 会被直接忽略（实测：0 报错、不落文件，而且仿真明显更快：
`--no-wave` 时 4.6~5.6 ms/s，出 FST 时 1~2 ms/s）。

| 想做的事 | 命令 |
|---|---|
| 出波形并自动打开（默认 FST） | `tools/em_tb/run_wave.sh` |
| 出波形但不打开（CI / 只想留文件） | `tools/em_tb/run_wave.sh --no-open` |
| 出 VCD（供 `vcd_check.py` 自动检查） | `tools/em_tb/run_wave.sh --vcd --no-open` |
| **不出波形**（只要 PASS/FAIL 与打印日志） | `tools/em_tb/run_wave.sh --no-wave` |

想改"dump 哪些信号"，编辑 `tb_exact_match.v` 里的 `$dumpvars`：
`$dumpvars(1, tb)` 是顶层（含 DUT 全部端口），紧跟着那串 `tb.dut.xxx` 是额外补的内部信号
（流水线 valid、`sState`、计数器等）。把那一串删掉就只留端口，波形文件更小。

## 打开波形

```bash
surfer out/em_tb/em_tb.fst                                  # 直接打开
surfer -c tools/em_tb/em_wave.sucl out/em_tb/em_tb.fst      # 带预设视图（脚本默认就是这条）
```

`em_wave.sucl` 里已经分好组（时钟握手 / 查找流水线 / svc引擎 / 计数与状态）并 `zoom_fit`。
在里面调好视图后可以 `save_state_as xxx.surf.ron` 存下来，下次 `surfer -s xxx.surf.ron` 复用。

FST / VCD 都是通用波形格式，换查看器不用重新仿真：

| 查看器 | 装法 | 说明 |
|---|---|---|
| **Surfer** | `brew install surfer` | 原生 arm64；同一套代码也有 VS Code 扩展与浏览器版 |
| VaporView | VS Code 扩展 `lramseyer.vaporview` | 免费开源，读 VCD / FST / GHW |
| WaveTrace | VS Code 扩展 `wavetrace.wavetrace` | 免费限 8 个信号，**只读 VCD** |
| GTKWave | ⚠️ macOS 的 Homebrew cask 已停用（上游 discontinued） | 要走社区 tap 或源码编 GTKWave 4 |

## 自动检查

```bash
tools/em_tb/run_wave.sh --vcd --no-open
python3 tools/em_tb/vcd_check.py out/em_tb/em_tb.vcd
```

会核对 **#(key 握手) == #(rsp_valid 高电平拍)**。这条检查是有来历的：`rsp_valid` 曾经
在 svc 冻结流水线时被保持多拍（一个请求被数成多个响应），就是 review 波形时这样发现的。
（FST 是二进制，这个脚本只吃 VCD；要自动检查就加上 `--vcd`。）

