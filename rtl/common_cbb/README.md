# common_cbb — 原始 Verilog CBB 参考 RTL 归档

本目录是 **2015 年手写的 Verilog 基础电路单元库（CBB）原始源码的归档**，原位于仓库外的
`Code-Repos/common_cbb/`，2026-09-13 整合进本仓库统一管理。

## 定位

| 维度 | 说明 |
|------|------|
| **性质** | 历史参考实现，**不参与 sbt 构建**，不在 `sbt test` 回归范围内 |
| **用途 1** | `BaseCbb`（Chisel 重写版）的**语义对照基线** —— 重写有歧义时以此为准 |
| **用途 2** | 后端**原语来源** —— `BaseCbb` 中若干 `BlackBox` 需要外部 Verilog 实体 |
| **来源** | 作者 Ethan Hao，2015-09 ~ 2015-10 编写，Windows 工程（原路径 `E:/project/common_cbb/`） |
| **归档原则** | 内容**逐字节原样保留**（29/29 文件 md5 校验一致，含 CRLF 原样），仅丢弃 IDE 元数据与重复文件 |

> ⚠️ 这是**只读参考件**。需要修改功能时改 `src/main/scala/BaseCbb/`，不要改这里。

## 目录结构

```
rtl/common_cbb/
├── common_cbb.f            # Filelist（旧版，缺 ma_fv_builder_mux.v）
├── common_cbb(1).f         # Filelist（较新，含 ma_fv_builder_mux.v）★ 推荐用这个
├── tb_pll.v                # PLL 例化的测试台
├── async/                  # 跨时钟域
│   ├── async.vsdx          #   Visio 设计图
│   ├── synchronizer.v      #   多位同步器
│   ├── async_bus.v         #   异步总线（数据 + 握手）
│   └── async_hs.v          #   脉冲跨时钟域握手同步
├── dff/                    # 触发器
│   ├── dff.v               #   最小 DFF 示例（模块名 `a`，测试残片）
│   └── dff.f
├── fifos/                  # FIFO
│   ├── sync_fifo.v         #   同步 FIFO（可编程 aful/aempt 门限 + 上溢/下溢标志）
│   ├── async_fifo.v        #   异步 FIFO（格雷码指针）
│   └── sync_pre_fifo.v     #   预取同步 FIFO（补偿 SRAM 读延迟 + 小 cache）
├── reg/
│   └── cg_rg.v             # valid 门控寄存器
├── sch/                    # 调度/移位
│   ├── rr.v                #   Round-Robin 轮询仲裁
│   └── barrel_shifter.v    #   桶形移位器
├── shifter/
│   ├── barrel_shifter_gen.pl  # bs*.v 的 Perl 生成脚本（参数 = 位宽）
│   ├── bs8.v ~ bs1024.v       # 8/16/32/64/128/256/512/1024 位左移桶形移位器
│   └── ma_fv_builder_mux.v    # profile 驱动的源数据→目标数据字节合并 mux
└── sim_ram/                # 存储器行为模型
    ├── sp_sim_ram.v        #   单口 SIM RAM
    ├── tp_sim_ram.v        #   伪双口 SIM RAM（1 写 + 1 读）
    ├── dp_sim_ram.v        #   真双口 SIM RAM（2 组独立读写口）
    └── ram_rdat_cg.v       #   读数据保持/门控包装（FLOP_OUT 可选）
```

## 与 BaseCbb 的对照关系

### 已被 Chisel 版覆盖

| 本目录文件 | BaseCbb 对应实现 | 备注 |
|---|---|---|
| `fifos/sync_fifo.v` | `BaseCbb.fifo.SyncFifo` / `SyncZeroLatencyFifo` | Chisel 版存储外置（经 `TpMemoryPort`）；原版可编程 `aful`/`aempt` 门限未 1:1 保留 |
| `fifos/async_fifo.v` | `BaseCbb.fifo.AsyncFifo` / `AsyncFifoCore` | |
| `sch/rr.v` | `BaseCbb.arbiter.RR` / `RrLogic` | |
| `sch/barrel_shifter.v`、`shifter/bs*.v` | `BaseCbb.math.LeftShifter` / `RightShifter` | Chisel 版为参数化实现，不再需要按位宽生成 8 个文件 |
| `async/synchronizer.v` | `BaseCbb.async.SynchronizerShiftReg` 族 | |
| `async/async_bus.v` | `BaseCbb.async.AsyncBus` | |
| `async/async_hs.v` | `BaseCbb.async.PulseSync` / `AsyncPulse` / `Handshake` | |
| `sim_ram/sp_sim_ram.v`、`tp_sim_ram.v` | `BaseCbb.memory.SimMemory` | |
| `reg/cg_rg.v` | `BaseCbb.basic.Register` / `RegEn` | |
| `dff/dff.v` | `BaseCbb.basic.DFF` / `DFFAsyncRst` | |

### 尚未 Chisel 化（缺口）

| 本目录文件 | 缺失能力 | 影响 |
|---|---|---|
| `fifos/sync_pre_fifo.v` | 预取 FIFO：`MEM_RD_LATENCY` 拍 SRAM 延迟补偿 + `CACHE_DEPTH = MEM_RD_LATENCY+1` 缓存 | 需要"读延迟 > 1 且要求零等待出队"的场景无现成件 |
| `sim_ram/dp_sim_ram.v` | 真双口 RAM（两组独立读写口） | `BaseCbb.memory.SimMemory` 只有单写单读（`TpMemoryPort`） |
| `sim_ram/ram_rdat_cg.v` | 读数据 flop 保持 / `cg_en` 门控包装，`FLOP_OUT` 可选 | 读数据路径需要门控或跨周期保持时无现成件 |
| `shifter/ma_fv_builder_mux.v` | profile（dest ID + byte enable）驱动的字节级数据合并 | 业务专用件，非通用 CBB |

> 这 4 个模块若要补齐，需在 `src/main/scala/BaseCbb/` 内用 Chisel 重写并配 chiseltest 回归。

### 归档中不包含

`BaseCbb.basic.ClockDivider2` / `ClockDivider3` / `Pow2ClockDivider` 是 `BlackBox`，
**本归档中没有对应的 Verilog 实体** —— 这三个仍需外部提供原语。

## 使用方法

源文件是**只读参考**，直接阅读即可。若需在本地用 Verilator/VCS 编译（例如做 Verilog↔Chisel 对拍），
`.f` 里是 Windows 绝对路径，需先转换：

```bash
# 从仓库根目录执行：E:/project/common_cbb/X  →  ./rtl/common_cbb/X
sed 's#E:/project/common_cbb/#./rtl/common_cbb/#' \
  rtl/common_cbb/common_cbb'(1)'.f > /tmp/common_cbb_local.f

verilator --lint-only -f /tmp/common_cbb_local.f
```

注意：`*.v` 在本仓库 `.gitignore` 中默认被忽略（该规则是为 `generated/` 下的 Chisel 生成物设计的），
本目录通过 `!rtl/**` 例外规则入库，新增文件会自动被跟踪，无需 `git add -f`。

`.gitattributes` 对 `rtl/**` 设置了 `-text`，**禁止一切行尾规范化**：原件是 2015 年 Windows 工程产物，
含 CRLF/LF 混合行尾（如 `fifos/sync_fifo.v` = 142 个 CRLF + 10 个裸 LF），Git 默认的 `core.autocrlf=input`
会把它们转成 LF 从而破坏归档真实性。已逐文件校验 git blob 与原件 md5 一致。

## 归档记录（2026-09-13）

**纳入（29 个文件，全部 md5 校验一致）**：上述树中全部 `.v` / `.pl` / `.f` / `.vsdx`。

**补档（同日，第二次）**：首次归档时实际只复制进来 16 个文件，本 README 的目录树所列的另外 13 个
（`common_cbb(1).f`、`dff/dff.v`、`dff/dff.f`、`shifter/barrel_shifter_gen.pl`、`shifter/bs8~bs1024.v`（8 个）、
`shifter/ma_fv_builder_mux.v`）在源目录里被漏掉，导致"README 声称 29 个 / 磁盘只有 17 个"的缺口。
已从**原库（原 `Code-Repos/common_cbb/`，当时已移入废纸篓，位于 `~/Library/Mobile Documents/.Trash/common_cbb/`）
的完整副本**补齐：13/13 与源副本逐字节 md5 一致，CRLF 原样保留，`cp -p` 保持时间戳。
至此归档 = 29 个源文件 + 本 README，与本文档描述完全一致。

> 补档时同时也验证过 `IdeaProjects/hardware-design/src/common_cbb/` 里的另一份副本：内容与原件
> 去行尾 CR 后完全相同，但该仓库**没有 `.gitattributes`**，CRLF 已被 `core.autocrlf=input` 归一化成 LF，
> 因此**不可**用作字节保真的来源（只能用于内容比对）。

**丢弃（9 项）**：

| 文件 | 原因 |
|---|---|
| `.DS_Store` | macOS 系统垃圾 |
| `.project`、`.svproject`、`.settings/org.eclipse.ltk.core.refactoring.prefs`、`.refactorings/**`（4 文件） | 旧 Eclipse/SV 编辑器工程元数据，无源码价值 |
| `sim_ram/sp_sim_ram(1).v` | 与 `sim_ram/sp_sim_ram.v` 仅 tab/空格缩进差异，语义完全相同 |

**已知的原件瑕疵（保持原样，未修正）**：

- `common_cbb.f` 与 `common_cbb(1).f` 内容不同：后者多一行 `shifter/ma_fv_builder_mux.v`，即 **(1) 是较新版本**。
- `common_cbb(1).f` 与 `common_cbb.f` 均无结尾换行。
- `dff/dff.v` 中模块名是 `a`（非 `dff`），疑为随手写的测试残片。
- `reg/cg_rg.v` 引用未定义的 `DATA_W`（无 `parameter` 声明），单独编译会报错；推测依赖上级文件定义或宏。
- `sim_ram/ram_rdat_cg.v` 的端口声明用 `clk`，但内部 `always` 块写的是 `i_clk`，**前后不一致**；该文件无法直接编译，仅作设计意图参考。
