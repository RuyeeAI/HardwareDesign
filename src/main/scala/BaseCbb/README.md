# BaseCbb Hardware Design Library

BaseCbb 是一个基于 Chisel 的硬件设计基础库，提供常用电路模块的 RTL 实现，包括：基本门级单元、时序单元、跨时钟域单元、仲裁器、存储器/FIFO、算术单元、寄存器块 DSL 和 Clos 网络。

> **详细设计文档见 [`docs/BaseCbb_设计文档/`](../../../docs/BaseCbb_设计文档/README.md)**（每子包一篇，含接口/行为/依赖/质量注意）。
> **功能重复分析见 [`docs/BaseCbb_设计文档/功能重复分析与修改建议.md`](../../../docs/BaseCbb_设计文档/功能重复分析与修改建议.md)**。
> **重构执行记录见 [`docs/BaseCbb_模块整理建议.md`](../../../docs/BaseCbb_模块整理建议.md)**。

---

## 包结构（与实际代码一致，2025-08 实测）

| 路径 | 包 | 内容 |
|------|----|------|
| `basic/` | `BaseCbb.basic`（部分 `BaseCbb.sequential`/`BaseCbb.utils.cdc`，见下） | 门级单元、寄存器、寄存器堆、计数器、分频器、FSM |
| `async/` | `BaseCbb.async`（`SynchronizerReg.scala` 为 `BaseCbb.utils.cdc`） | 同步器、脉冲跨越、异步总线、复位同步、握手 |
| `arbiter/` | `BaseCbb.arbiter`（`arbiter.scala` 为根包 `BaseCbb`） | RR / WRR / iSLIP / Hella 锁定仲裁器 |
| `memory/` | `BaseCbb.memory` | 存储配置与封装（SP/TP/ECC/Wrap3）、位图分配器（BitmapKernel/Bitmap/IDPool/BitmapCacheMem）、链表 |
| `fifo/` | `BaseCbb.fifo` | 同步/异步 FIFO（存储外置，经 Tp/SpMemoryPort 连接） |
| `math/` | `BaseCbb.math` | 加法器/比较器/乘法器/移位器、校验和、CRC、LFSR、压缩/分散、计数器、查表、前缀和 |
| `misc/` | `BaseCbb.misc`（`Timer.scala`/`Shaper.scala` 为 `BaseCbb.utils.timer`） | 延迟队列族、分发/重发、移位寄存器、组合工具（MuxT/Str/Random 等） |
| `io/` | `BaseCbb.io` | 主机侧工具：文件、JSON、生成期 PRNG、结构化相等 |
| `data/` | `BaseCbb.data` | GenBundle/GenModule（全库基类）、异构 Record 容器 |
| `annotation/` | `BaseCbb.annotation` | 后端 FIRRTL 注解（SRAM/中断/参数） |
| `Area/` | `BaseCbb.Area` | 工艺面积估算 |
| `Clos/` | `BaseCbb.Clos` | Benes 置换网络 |
| `RegCbb/` | `BaseCbb.RegCbb(.dsl/.hw/.gen/.demo)` | 寄存器文件框架（v2）：IR → 地址分配 → RTL → 用户视图 → 文档/软件视图生成 |

⚠ **已知包名/目录错位**（P3 迁移遗留，修改建议见 [功能重复分析与修改建议 §7](../../../docs/BaseCbb_设计文档/功能重复分析与修改建议.md)）：`basic/` 下的 `SequentialUnits.scala`/`ClockDivider.scala` 声明 `BaseCbb.sequential`，`AsyncResetReg.scala` 声明 `BaseCbb.utils.cdc`；`misc/` 下的 `Timer.scala`/`Shaper.scala` 声明 `BaseCbb.utils.timer`；`async/SynchronizerReg.scala` 声明 `BaseCbb.utils.cdc`；`arbiter/arbiter.scala` 声明根包 `BaseCbb`。

---

## 模块速览（每子包详见对应设计文档）

### basic/ — 门级与时序

| 类别 | 模块 |
|------|------|
| 门级 | `Inv/Buf/And2/And3/Nand2/Nand3/Or2/Nor2/Nor3/Xor2/Xnor2/Mux2N/HalfAdd/FullAdd/AOI22/AOI32/SRLatch/ClockGating` |
| 时序 | `DFF/DFFAsyncRst/DFFSyncRst/DLatch/Register/RegFile/RegFile1R1W/RegFile2R1W` |
| 计数器 | `UpCounter/ModNCounter`（后者包装 `math.ZCounter`） |
| 分频 | `ClkDiv2/ClkDivOdd/ClkDiv`（行为级）、`ClockDivider2/ClockDivider3/Pow2ClockDivider`（BlackBox，需外部 Verilog） |
| FSM | `FsmTemplate`（三态模板） |
| 异步复位寄存器 | `AsyncResetReg/AsyncResetRegVec`（包 `BaseCbb.utils.cdc`，被 misc.ShiftReg 复用） |

### async/ — 跨时钟域

| 类别 | 模块 |
|------|------|
| 同步器 | `Sync2`（委托 `utils.cdc.AsyncResetSynchronizerShiftReg`）、`Sync`（SYNC_FF BlackBox） |
| CDC 原语族 | `SynchronizerShiftReg/AsyncResetSynchronizerShiftReg/ResetSynchronizerShiftReg/ClockCrossingReg`（包 `BaseCbb.utils.cdc`，全部基于 misc.AbstractPipelineReg） |
| 脉冲/数据跨越 | `PulseSync`（无反馈）、`AsyncPulse`（4 相握手）、`AsyncBus[T]` |
| 复位同步 | `AsyncRstSync`、`ResetCatchAndSync`（+ PSDTestMode） |
| 其他 | `Handshake[T]`、`GrayCounter`、`EdgeDetect`、`Blockable`/`BlockDuringReset` |

### arbiter/ — 仲裁器

- 公共原语：`RrLogic`（根包，双份向量借位 RR 公式）
- `RR`（轮询）、`WRR`（权重门控 + RR）、`HellaLockingArbiter` 族（HellaPeeking/HellaCounting，rocket-chip 移植）、`iSlipLogic`/`RegulariSlip`（iSLIP 两阶段调度）

### memory/ — 存储与分配

- 配置/封装：`Memory`（case class）、`SpMemoryPort`/`TpMemoryPort`（± Lgc 端口）、`SpMemoryBB`/`TpMemoryBB`、`SimMemory`、`SpMemoryWrap`/`TpMemoryWrap`、`SpMemoryWrap3`/`TpMemoryWrap3`（ECC/Parity + DFX + CPU 仲裁）、`EccCodec`
- 分配器：`BitmapKernel`（共享组合内核，1=可用）→ `Bitmap`/`IDPool`/`BitmapCacheMem`
- 链表：`SubLinklist`/`LinkList`/`VoqLinkList`

### fifo/ — FIFO（存储外置）

- `SyncFifo`（readLatency 0/1）、`SyncZeroLatencyFifo`、`RegisterBasedFifo`、`DualSPRamFifo`/`DualSinglePortRamFifo`
- `AsyncFifo`（格雷码指针）、`AsyncZeroLatencyFifo`

### math/ — 算术与数学

- 算术：`RippleCarryAdder/CarrySelectAdder/Subtractor/AddSub/Comparator/Multiplier/LeftShifter/RightShifter`
- 校验/PRNG：`Checksum`（RFC 1071）、`Crc`/`Icrc`、`Lfsr`
- 数据通路：`Compress`/`Scatter`（基于 DensePrefixSum）、`RipplePrefixSum`/`DensePrefixSum`/`SparsePrefixSum`、`ReduceOthers`
- 计数/查表：`ZCounter/TwoWayCounter/WideCounter`、`MuxLiteral/MuxSeq/MuxTable`

### misc/ — 通用构件

- 延迟族：`LatencyPipe`/`LatencyPipeV`/`RegEn`、`DelayQueue`、`ReorderQueue`、`ShiftQueue`
- 分发：`Broadcaster`（1→N）、`Repeater`（重复发送）
- 定时/整形：`Timer/SimpleTimer/DynamicTimer`、`Shaper`（令牌桶）
- 移位/流水：`ShiftRegInit`、`AbstractPipelineReg`、`AsyncResetShiftReg`
- 组合工具：`DecoupledHelper/MuxT/MuxTLookup/ValidMux/Str/Split/Random/Majority/PopCountAtLeast/MaskGen`
- 向量工具：`Seq2Vec/SubVec/Convert2dArray`、`GenProcessBuilder`

### io/ · data/ · annotation/ · Area/ · Clos/

- `io/`：`ReadFile/WriteFile/ReadStdIO`、`JsonTools`、`SeededRandom`、`SimpleProduct`（主机侧）
- `data/`：`GenModule/GenBundle/fldAttr`、`HeterogeneousBag`、`RecordMap`
- `annotation/`：`SRAMAnnotation/InterruptsPortAnnotation/GlobalConstantsAnnotation/ParamsAnnotation` + `Annotated`
- `Area/`：`ProcessConfiguration`、`GenArea`
- `Clos/`：`BenesClos2x2/BenesClos/Benes`

### RegCbb/ — 寄存器文件框架（v2）

```
Def（IR：RegBlockDef/RegDef/RegFieldDef/MemoryDef + AccessType/HwAction）
  → AddressAllocator（字段位/寄存器字节/存储器基址）
  → hw/RegCore（FieldReg 单寄存器 + RegFileTop 顶层 + MemPortIO 请求-响应 + MemStatus）
  → hw/RegView（用户连接视图：命名访问 + 位域切割）
  → hw/AxiLite（AxiLiteRegFile 包装）
  → gen/（JsonGen/CHeaderGen/MarkdownGen/HtmlGen/ViewSourceGen）
  → demo/UartDemo（完整示例，EmitAll 一键生成）
```

支持：8 种字段访问类型（RW/RO/WO/RC/RS/W1C/W1S/W1T）、>32bit 多字寄存器（原子/非原子）、
宽 memory 地址空间（原子/非原子总线访问）、Markdown/HTML/JSON/C 文档生成。
**系统级扩展**：RegBlock（纯寄存器块）/ MemBlock（纯存储器块）分离组合 → FuncModule（功能模块，多块）→
System（系统，多模块）；模块间地址自动/手工分配（`AddressAllocator.allocateSystem`）；
`SystemRegFileTop` 模块间译码分发汇聚 + `SystemRegView` 三级命名访问 + 系统级文档生成器（`gen/SystemGen`）。
详见 [`docs/RegCbb_系统级设计文档.md`](../../../docs/RegCbb_系统级设计文档.md)、
[`docs/BaseCbb_设计文档/13_RegCbb.md`](../../../docs/BaseCbb_设计文档/13_RegCbb.md) 与
[`docs/寄存器编写指导.md`](../../../docs/寄存器编写指导.md)。

---

## 验证

```
sbt compile   ✓
sbt test      211/211 通过（35 suites）
```

## 附录：模块依赖关系

```
basic（门级/时序/分频）
   ├─► math（ArithmeticUnits ← basic.FullAdd；Counters ← SequentialUnits 反向）
   │     ├─► PrefixSum ─► Compress/Scatter；Crc ─► Icrc；Lfsr；Checksum；MuxLiteral；ReduceOthers
   ├─► misc（ShiftReg ← basic.AsyncResetRegVec；MuxT/MuxTLookup ↔ math.MuxTable 重叠）
   ├─► async（Sync2/AsyncBus/AsyncPulse/ResetCatchAndSync ← utils.cdc.AsyncResetSynchronizerShiftReg ← misc.AbstractPipelineReg）
   ├─► arbiter（RrLogic ← 根包；iSlipLogic/RegulariSlip ← data.GenModule + misc.Seq2Vec）
   ├─► memory（BitmapKernel ─► Bitmap/IDPool/BitmapCacheMem；Memory ─► Sp/TpPort ─► fifo；Linklist ← data.GenModule）
   ├─► data（GenBundle/GenModule ─► 全库）
   ├─► io / annotation / Area / Clos / RegCbb（RegCbb 独立）
```
