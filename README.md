# HardwareDesign 基础电路单元库 (Chisel 版本)

使用 [Chisel](https://github.com/chipsalliance/chisel3) 硬件构造语言实现数字 IC 设计中常用的
基础电路单元、存储子系统、寄存器框架与网络处理模块。

## 环境

- sbt 1.9+（Scala 2.13.16 / Chisel 7.15.0，`org.chipsalliance`）
- 仿真：`chisel3.simulator`（Verilator 后端，随 Chisel 提供，不再依赖已停止维护的 chiseltest）
- **firtool 无需手动安装**：chisel 7.15.0 钉 firtool 1.158.0，firtool-resolver 会自动下载并缓存；
  要用自带版本就设 `CHISEL_FIRTOOL_PATH` 指向包含 firtool 的目录（PATH 上版本不匹配的旧 firtool
  会被 chisel 忽略，但**不要**在自己的脚本里手动调它——1.62.0 解析不了 chisel7 的 CHIRRTL）。
- **TestCase 注意**：`chisel3.simulator` 不像旧的 treadle 后端那样在 t=0 就给出 `RegInit` 值，
  依赖复位初值的用例必须先调 `BaseCbb.SimReset(dut)`（见 `src/test/scala/BaseCbb/SimReset.scala`）。
- **随机化语义**：chisel 7 的 ChiselSim 默认把未初始化寄存器/存储上电为**随机值**；
  本仓库统一用 `BaseCbb.Sim.simulate`（= `EphemeralSimulator` + `Randomization.uninitialized`）
  保持 chisel5 的"未复位为 0"语义，见 `src/test/scala/BaseCbb/Sim.scala`。

## 快速上手

```bash
sbt test                        # 全量回归
sbt "runMain BaseCbb.memory.EmitMemVerilog"   # 生成示例 SRAM Verilog 到 generated/
sbt "runMain HBS.swf.SwfMain"   # HBS 顶层 SwfCore 的 Verilog 生成（需大堆，见下）
```

所有 `EmitXxx` 入口的产物统一写入 `generated/`（已 gitignore，可随时重新生成）。

> HBS 的重顶层（`SwfCore` / `SfuTop` / `SfuCorner` / `SfuMid`）实例化规模极大，
> elaboration 需 >6GB 堆，故不在默认 `sbt test` 内；需要时 `sbt -J-Xmx8G "runMain HBS.swf.SwfMain"`。


## 模块索引

### BaseCbb — 基础电路单元库（`src/main/scala/BaseCbb/`）

| 子包 | 内容 |
|------|------|
| `basic/` | 门级单元（Inv/And/Nand/Mux/译码器/DFF/锁存器/AOI）、时序单元、分频器 |
| `math/` | 加法器/乘法器/移位器、前缀和、CRC/LFSR/Checksum、压缩网络、计数器 |
| `misc/` | LatencyPipe、DelayQueue、ShiftQueue、ReorderQueue、Shaper、Timer 等数据通路小件 |
| `memory/` | SRAM 封装（Sp/Tp Wrap/Wrap3，含 ECC/Parity、DFX、CPU 访问）、位图、链表、IDPool |
| `fifo/` | 同步/异步 FIFO（多存储后端） |
| `async/` | CDC 同步器、脉冲同步、异步复位同步（行为级原语 + desiredName 供后端替换） |
| `arbiter/` | RR/WRR/iSLIP 仲裁器 |
| `data/` | GenModule/GenBundle 基类、Record 容器、★ 设计→IR 导出机件（`GenParam`/`GenIR`/`GenDataStructure`/`GenMemory`/`GenFieldListFromBundle`） |
| `io/` | 主机侧文件/JSON/随机工具 |
| `annotation/` `Area/` `Clos/` `CBFC/` | 后端注解、面积估算、Benes Clos 网络、信用流控发送端口（WIP） |
| `RegCbb/` | ★ 寄存器框架：DSL 定义 → 地址分配 → RTL → JSON/C 头/Markdown/HTML 生成 |

### FPP — 网络处理（`src/main/scala/FPP/`）

- `Parser/`：多协议报文头解析流水线（ETH/VLAN/MPLS/IPv4/IPv6/TCP/UDP/GRE/隧道等）
- `OSA/OSM/`：输出侧调度/组包（分段、上下文分配、缓存、信元组装、出口调度、反压）
- `Table.scala` + `LB/cfg/{DataStruct, LbTableDefinition}.scala`：ECMP/LB 表项定义（`GenBundle` + `fldAttr` 描述）
- `FvProfile/`：FV（field vector）profile 划分的纯 Scala 模型

### Demo — 「参数→IR→JSON」演示（`src/main/scala/Demo/`）

`GenParam` + `GenMemory`/`GenIR`/`GenDataStructure` 的活样例：`sbt "runMain Demo.Main"` 生成
`generated/DemoIR.json`。`TestSram` 演示 `DescribedSRAM`（带描述信息的 SRAM，
描述在 elaboration 期打印；chisel 7 移除了 FIRRTL 注解机制，原 annotation 记录方式随之删除）。

### Perf — 性能模型（`src/main/scala/Perf/`，纯 Scala，无 Chisel）

`Perf/common/`：仲裁器 / CAQM / 延迟线 / 包生成器 / 性能监视器 / Shaper；
`Perf/FPP/`：EPP 数据通路（`EppDatapath`、`EPP_TC0`、`S93_EPP`、`AbsPfc`）。
入口 `Perf.FPP.Main` / `absTest` / `xTest`，CSV 产物写 `generated/`。

### HBS — 高带宽交换（`src/main/scala/HBS/`）

2026-09-13 由 `gitee.com/ethanhao/HighBandwidthSwitching`（分支 master @ 8fdd933）整合进来，
只保留其独有 RTL；仓库里那份重复的 `BaseCbb` 已弃用，统一用本仓库的 `BaseCbb`。

- `top/` `tm/`：HBS 全局参数（`HbsParams`）与流量管理参数/结构（`TmParam`、`PacketLinkList`）
- `adm/`：报文聚合分发（`HbsAdm`，WIP：IO 尚未展开）
- `swf/`：交换阵列主体 —— `swf_top/SwfCore`+`SfuTop`、`sfu_corner/SfuCorner`+`SfuBuffer`+`SfuControllor`、
  `sfu_mid/SfuMid`、`sfu_routing/SfuRouting`+`SfuRoutDatapath`、`common/`（`BusMatrix`、`BusSelection`、
  `SfuiSlip`、`VoqBuffer`、`Bundles`、`SwfInterface`、`SwfParams` 等）
- 设计图随代码入库（`HBS.svg` / `SWF.svg` / `SfuCorner.svg` 等），沿用 BaseCbb 的既有约定

移植时相对 HBS 的改动（完整清单见 `.workbuddy/memory/2026-09-13.md`）：
`BaseCbb.utils.*` → `BaseCbb.misc/io/data`；`Memory(..., "1R1W")` → `MemoryAccessType.TP`；
`log2Up` → `log2Ceil`；`GenParam` 补回 `BaseCbb.data.GenParam`（HBS 侧原 `BaseCbb/utils/GeneratorLib.scala` 其余部分无人引用，未迁）。

### 其他

- `ImpulseGenerator/`：受控脉冲发生器

## 参考文档（`docs/`）

`UET.md`、`Hotchips 2024.md`、`FlowControl.svg`、`UB.svg`、`UEC.svg` 等为 2026-09-13 从
`IdeaProjects/hardware-design`（gitee `ethanhao/hardware-design`）收编的阅读笔记与参考图。

## 参考 RTL 归档（`rtl/`）

`rtl/common_cbb/` 存放 **2015 年手写的 Verilog 基础单元库原始源码**（作者 Ethan Hao），2026-09-13 由仓库外
`Code-Repos/common_cbb/` 整合进来统一管理。

- 定位：`BaseCbb` 的**语义对照基线** + 后端 `BlackBox` 的**原语来源**。
- **不参与 sbt 构建**，不在 `sbt test` 范围内；`.gitignore` 通过 `!rtl/**` 例外规则入库。
- 其中 `sync_pre_fifo`（预取 FIFO）、`dp_sim_ram`（真双口 RAM）、`ram_rdat_cg`（读数据门控）、
  `ma_fv_builder_mux`（字节合并 mux）**尚未 Chisel 化**，详见归档 README。

详见 [`rtl/common_cbb/README.md`](rtl/common_cbb/README.md)（含逐模块对照表与归档记录）。

## 文档

- `docs/BaseCbb_设计文档/` — 按子包的设计说明与《功能重复分析与修改建议》
- `docs/BaseCbb/RegCbb/docs/寄存器编写指导.md` — RegCbb 寄存器编写与外围逻辑连接指导
- `docs/OSA.md` / `docs/PreParser.md` — FPP 各模块设计
- `docs/工程优化建议_2026-08-28.md` — 全工程评审与优化记录

## 已包含单元（节选）

### 基础门级 / 时序（`basic/`）
Inv、Buf、And2/3、Nand2/3、Or2/Nor2/3、Xor2/Xnor2、Mux2/Mux2N、Dec2/Dec3、
DLatch、DFF（异步/同步复位）、半加器/全加器、SR 锁存器、时钟门控、AOI22/32；
Register、RegFile1R1W/2R1W、Up/ModN 计数器、ClkDiv2/ClkDivOdd/ClkDiv、SyncFifo、三段式 FSM 模板。

### 算术（`math/`）
RippleCarry/CarrySelect 加法器、减法器、AddSub、比较器、乘法器、移位器；
前缀和、压缩网络、CRC、LFSR、Checksum。

### 存储（`memory/`）
SpMemoryWrap/TpMemoryWrap（插拍流水）、Sp/TpMemoryWrap3（ECC/Parity + 初始化 + CPU 访问 + 错误注入）、
SimMemory、位图/链表/IDPool。

### 寄存器（`RegCbb/`）
字段级 DSL（RO/RW/W1C/W1S/W1T/RC/RS）、原子多字寄存器、AXI-Lite 适配、
一键生成 JSON / C 头 / Markdown / HTML 寄存器文档（demo 见 `RegCbb/demo/UartDemo.scala`）。

## CI

`.github/workflows/ci.yml` 在 push/PR 时运行 `sbt -batch test`。
