# BaseCbb 设计文档总览

> 本目录为 `src/main/scala/BaseCbb/` 下所有模块的设计文档。
> 每个子包一篇文档，覆盖：模块清单、接口定义、行为语义、依赖关系、设计注意与验证情况。
> 配套文档：`docs/BaseCbb_模块整理建议.md`（重构执行记录）、`docs/功能重复分析与修改建议.md`（重复分析）。

---

## 文档导航

| 文档 | 覆盖目录 | 包 |
|------|----------|----|
| [01_basic.md](01_basic.md) | `basic/` | `BaseCbb.basic`、`BaseCbb.sequential`、`BaseCbb.utils.cdc`（部分） |
| [02_async.md](02_async.md) | `async/` | `BaseCbb.async`、`BaseCbb.utils.cdc`（部分） |
| [03_arbiter.md](03_arbiter.md) | `arbiter/` | `BaseCbb.arbiter`、根包 `BaseCbb` |
| [04_memory.md](04_memory.md) | `memory/` | `BaseCbb.memory` |
| [05_fifo.md](05_fifo.md) | `fifo/` | `BaseCbb.fifo` |
| [06_math.md](06_math.md) | `math/` | `BaseCbb.math` |
| [07_misc.md](07_misc.md) | `misc/` | `BaseCbb.misc`、`BaseCbb.utils.timer`（部分） |
| [08_io.md](08_io.md) | `io/` | `BaseCbb.io` |
| [09_data.md](09_data.md) | `data/` | `BaseCbb.data` |
| [10_annotation.md](10_annotation.md) | `annotation/` | `BaseCbb.annotation` |
| [11_Area.md](11_Area.md) | `Area/` | `BaseCbb.Area` |
| [12_Clos.md](12_Clos.md) | `Clos/` | `BaseCbb.Clos` |
| [13_RegCbb.md](13_RegCbb.md) | `RegCbb/` | `BaseCbb.RegCbb(.dsl/.hw/.gen/.demo)` |

---

## 设计原则

1. **单一职责**：每个模块只做一件事（门级、同步器、仲裁、存储、FIFO、算术、寄存器 DSL 互不混叠）。
2. **接口显式**：端口命名规范（`*_vld/*_rdy`、`*_en`、`*_ptr`），方向从"模块向外看"。
3. **存储外置**：FIFO 的 SRAM 一律通过 `TpMemoryPort`/`SpMemoryPort` 外挂，模块本体可仿真可综合。
4. **共享内核**：重复的组合逻辑抽取为共享内核（如 `memory/BitmapKernel`、`math/PrefixSum` 前缀和）。
5. **单一事实源**：RegCbb 寄存器定义只写一次，RTL/文档/软件视图全部由 IR 生成。

---

## 依赖总览

```
BasicCells（门级）
   ├─► SequentialUnits（寄存器/计数器/分频）──► ClockDivider
   ├─► ArithmeticUnits（算术）◄── basic.HalfAdd/FullAdd
   ├─► math/（PrefixSum ─► Compress/Scatter；Counters；MuxLiteral；Crc/Lfsr/Checksum）
   ├─► async/（SynchronizerReg 族 ◄─ ShiftReg/ShiftRegInit）
   ├─► arbiter/（RR/WRR ◄─ 根包；iSlip；Hella 族）
   ├─► memory/（Memory ─► Sp/TpMemoryPort ◄─ fifo/；BitmapKernel ─► Bitmap/IDPool/BitmapCacheMem）
   ├─► fifo/（SyncFifos/AsyncFifos ◄─ TpMemoryPort、async.Sync）
   ├─► misc/（LatencyPipe/ShiftQueue/DelayQueue/ReorderQueue/Broadcaster/Repeater/Shaper/Timer）
   ├─► data/（GenBundle/GenModule ◄─ 全库引用；HeterogeneousBag/RecordMap）
   ├─► io/（FileIO/JsonTools/SeededRandom/SimpleProduct）
   └─► RegCbb/（Def ─► AddressAllocator ─► hw.{RegCore,RegView,AxiLite} ─► gen.*）
```

---

## 现状速览（2025 实测）

- 主代码 **65 个文件 / 约 7569 行**；测试 **34 个 spec / 约 4027 行**（`sbt test` 全部通过）。
- **已知结构问题**：6 个文件的 package 声明与物理目录不一致（P3 迁移遗留），详见
  [功能重复分析与修改建议.md](功能重复分析与修改建议.md) §7。
- README.md（仓库根，`src/main/scala/BaseCbb/README.md`）已与实际结构脱节，需同步更新。
