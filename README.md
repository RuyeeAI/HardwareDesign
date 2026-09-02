# HardwareDesign 基础电路单元库 (Chisel 版本)

使用 [Chisel](https://github.com/chipsalliance/chisel3) 硬件构造语言实现数字 IC 设计中常用的
基础电路单元、存储子系统、寄存器框架与网络处理模块。

## 环境

- sbt 1.9+（Scala 2.13.12 / Chisel 3.6.1 / chiseltest 0.6.2）
- 仿真后端：verilator（`src/test` 中的 wrap 级测试需要；纯 Scala 侧测试无额外依赖）

## 快速上手

```bash
sbt test                        # 全量回归（41+ suites）
sbt "runMain BaseCbb.memory.EmitMemVerilog"   # 生成示例 SRAM Verilog 到 generated/
```

所有 `EmitXxx` 入口的产物统一写入 `generated/`（已 gitignore，可随时重新生成）。

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
| `data/` | GenModule/GenBundle 基类、Record 容器 |
| `io/` | 主机侧文件/JSON/随机工具 |
| `annotation/` `Area/` `Clos/` | 后端注解、面积估算、Benes Clos 网络 |
| `RegCbb/` | ★ 寄存器框架：DSL 定义 → 地址分配 → RTL → JSON/C 头/Markdown/HTML 生成 |

### FPP — 网络处理（`src/main/scala/FPP/`）

- `Parser/`：多协议报文头解析流水线（ETH/VLAN/MPLS/IPv4/IPv6/TCP/UDP/GRE/隧道等）
- `OSA/OSM/`：输出侧调度/组包（分段、上下文分配、缓存、信元组装、出口调度、反压）

### 其他

- `Feishu/`：飞书开放平台客户端（需本地 `feishu.conf`，参见 `feishu.conf.example`）
- `ImpulseGenerator/`：受控脉冲发生器

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
