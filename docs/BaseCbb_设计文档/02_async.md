# async/ — 跨时钟域（CDC）单元

> 路径：`src/main/scala/BaseCbb/async/`　包：`BaseCbb.async`（`SynchronizerReg.scala` 例外：声明 `BaseCbb.utils.cdc`，物理路径与包不一致，见 §8）
> 文件 9 个：AsyncBus / AsyncPulse / AsyncRstSync / AsyncUnits / Blockable / BlockDuringReset / ResetCatchAndSync / Sync / SynchronizerReg

---

## 1. 家族总览

| 功能族 | 模块 | 关键特性 |
|--------|------|----------|
| 同步器 | `Sync2`、`Sync`、`SynchronizerShiftReg` 族（utils.cdc） | 多级触发器链 |
| 脉冲跨越 | `PulseSync`（无反馈）、`AsyncPulse`（4 相握手有反馈） | toggle / req-ack |
| 数据跨越 | `AsyncBus[T]` | 2 相握手 + 组合直通（有缺陷，见 §8） |
| 复位同步 | `AsyncRstSync`、`ResetCatchAndSync` | 异步置位/同步释放 |
| 握手/计数 | `Handshake[T]`、`GrayCounter` | 单域 4 相握手；格雷码计数 |
| 边沿/阻塞 | `EdgeDetect`、`Blockable`/`BlockDuringReset` | 边沿检测；复位期阻塞 |

**依赖链**：`AsyncBus`/`AsyncPulse`/`PulseSync` → `Sync2` → `BaseCbb.utils.cdc.AsyncResetSynchronizerShiftReg` → `BaseCbb.misc.AbstractPipelineReg`；
`ResetCatchAndSync` → `AsyncResetSynchronizerShiftReg`；`BlockDuringReset` → `Blockable`。

---

## 2. Sync2 — 单比特同步器（AsyncUnits.scala）

```scala
class Sync2(depth: Int = 2)   // require(depth >= 2)
// clk, rst_n(AsyncReset, 低有效), din, dout
```
- `io.dout := AsyncResetSynchronizerShiftReg(io.din, depth, 0)` —— 复用 utils.cdc 原语
  （Async 复位、init=0），`desiredName` 编码 `AsyncResetSynchronizerShiftReg_w1_d{depth}_i0` 供后端识别替换。
- 设计意图：**避免两套同步链实现漂移**，全库 CDC 统一走该原语。

## 3. PulseSync / AsyncPulse — 脉冲跨越

| 特性 | `PulseSync`（AsyncUnits.scala） | `AsyncPulse`（AsyncPulse.scala） |
|------|------|------|
| 协议 | toggle 翻转 + XOR 边沿检测 | 4 相 req/ack 闭环 |
| 反馈 | **无** | **有**（ack 经 Sync2 同步回源域） |
| 延迟 | 约 3 拍 | 约 6+ 拍（握手往返） |
| 可靠性 | 脉冲间隔 < 同步延迟时会**合并/丢失**脉冲 | 不丢脉冲（未完成握手前新脉冲忽略） |
| IO | srcClk/srcRst_n/dstClk/dstRst_n/pulseIn/pulseOut | 同左 |

- `PulseSync`：源域 `toggle ^= pulseIn` → `Sync2` 同步 → 目标域 `pulseOut = sync ^ RegNext(sync)`。
- `AsyncPulse`：源域 `when(pulseIn) reqReg := true; when(ackSyncIn) reqReg := false`；目标域上升沿检测出脉冲。

## 4. AsyncBus — 异步数据总线

```scala
class AsyncBus[T <: Data](gen: T)
// srcClk/srcRst_n, dstClk/dstRst_n, srcValid, srcData, dstValid, dstData
```
- 源域：`reqToggle ^= srcValid`；数据 `dataReg := RegEnable(srcData, srcValid)`。
- req 经 `Sync2` 到目标域，XOR 检测产生 `dstValid` 脉冲。
- ⚠ **已知缺陷**：`dstData` 为源域寄存器**组合直通**（未同步，多比特一致性无保证）；ack 链路（`ackSyncIn`）已建但**从未消费**（半成品）。
- 建议：单比特/慢变化控制信号可用；多比特数据需外部配合格雷码或数据保持窗口；ack 通路要么实现等待要么删除。

## 5. Handshake — 单域 4 相握手（AsyncUnits.scala）

```scala
class Handshake[T <: Data](dataType: T)
// srcValid, srcReady, srcData, dstValid, dstReady, dstData
```
- 状态位 `regReq`/`regAck` 表达 4 相握手；数据锁存 `regData`。
- ⚠ `dstReady` **声明但从未参与逻辑**（自动应答式握手）——死端口，建议修复或删除。

## 6. GrayCounter — 格雷码计数器（AsyncUnits.scala）

```scala
class GrayCounter(width: Int = 4)   // clk, rst_n, en, binary, gray
```
- `binCnt` 递增计数；`gray = binCnt ^ (binCnt >> 1)` 组合转换。
- 要求 `en` 每拍至多 1 个脉冲（多拍计数会跳过格雷码转换，破坏单 bit 变化性质）。
- 用途：异步 FIFO 指针跨域。

## 7. EdgeDetect — 边沿检测（AsyncUnits.scala）

```scala
class EdgeDetect   // din → rising, falling, any（RegNext 延迟一拍）
```

## 8. 复位同步器：AsyncRstSync vs ResetCatchAndSync

| 特性 | `AsyncRstSync`（AsyncRstSync.scala） | `ResetCatchAndSync`（ResetCatchAndSync.scala） |
|------|------|------|
| 参数 | 无（固定 2 级） | `sync: Int = 3`（`desiredName = ResetCatchAndSync_d{sync}`） |
| 输出类型 | `AsyncReset`（高有效） | `Bool`（复位期间为 1） |
| PSD/DFT | 无 | `PSDTestMode`（test_mode/test_mode_reset 旁路） |
| 实现 | 手写两级 `RegInit(true.B)` | 复用 `AsyncResetSynchronizerShiftReg(true.B, sync)` 取反 |
| 便捷工厂 | 无 | companion `apply(clk, rst, sync, name, psd)` 系列 |

- 两者语义等价（异步置位、同步释放、高有效输出）。
- **建议**：保留功能更全的 `ResetCatchAndSync`，`AsyncRstSync` 删除或改为一层薄封装（内部调用 ResetCatchAndSync 并转 AsyncReset）。

## 9. Blockable / BlockDuringReset — 复位期阻塞

- `trait DataCanBeValid { val valid: Bool }`、`trait Blockable[T] { def blockWhile(enable_blocking, data): T }`。
- 隐式实例：`Bool`（`x && !enable_blocking`）、`DataCanBeValid`（只压 valid）、
  `DecoupledIO`（同时压 valid 与反向压 ready，完全停流）、`Vec[T]`（逐元素递归）。
- `BlockDuringReset.apply[T : Blockable](data, stretchCycles = 0)`：复位中及复位后 `stretchCycles` 拍阻塞事务。
- ⚠ `stretchCycles=0`（RegNext 分支）与 `=1`（Counter 分支）实际等待拍数几乎相同，参数语义（总拍数 vs 额外延伸拍数）文档缺失。

## 10. Sync / SYNC_FF — BlackBox 同步器（Sync.scala）

```scala
class SYNC_FF extends BlackBox   // clk, din, dout（无复位端口，后端单元库 lib_sync_ff）
class Sync(StageNum: Int = 2, Width: Int = 1)
// i_clk, i_data, o_data
```
- 每比特 `StageNum` 级 `SYNC_FF` 级联；无复位端口；多比特并行同步不保证一致性。
- 与 `Sync2`/`SynchronizerShiftReg`（行为级）**功能重复**（详见重复分析文档 §2.1）：
  若走"行为原语 + desiredName 后端替换"路线可删 `Sync`；若后端需要 `lib_sync_ff` 单元则保留 `Sync`。

## 11. SynchronizerReg.scala — CDC 原语族（`BaseCbb.utils.cdc`）

> ⚠ 文件物理位于 async/ 目录，包名是 `BaseCbb.utils.cdc`（P3 迁移遗留，见重复分析文档 §7）。

| 类 | 复位类型 | desiredName | 说明 |
|----|---------|-------------|------|
| `SynchronizerPrimitiveShiftReg`（private） | 参数化 | `{ResetType}ResetSynchronizerPrimitiveShiftReg_d{sync}[_i{init}]` | 1bit 行为级同步链基元 |
| `AsyncResetSynchronizerShiftReg(w, sync, init)` | Async（`RequireAsyncReset`） | `…_w{w}_d{sync}_i{init}` | **全库 CDC 主原语**（Sync2/ResetCatchAndSync 复用） |
| `ResetSynchronizerShiftReg(w, sync, init)` | Inferred | 同上 | 复位类型由 Chisel 推断 |
| `SynchronizerShiftReg(w, sync=3)` | NonSync（无复位） | `SynchronizerShiftReg_w{w}_d{sync}` | `sync==0` 时 companion 直通 bypass |
| `ClockCrossingReg(w, doInit)` | 单级带使能 | `ClockCrossingReg_w{w}` | `RegEnable`，跨域单拍打拍 |

- 全部继承 `BaseCbb.misc.AbstractPipelineReg`；companion 以"函数式" `apply[T <: Data]` 包装。
- 设计意图：行为级实现 + `desiredName` 编码，供后端替换为 metafix 触发器链。

## 12. 质量与一致性注意

1. **包/目录不一致**：`SynchronizerReg.scala` 声明 `BaseCbb.utils.cdc`，与同目录 8 个文件的 `BaseCbb.async` 不一致。
2. **两套后端替换机制并存**：`Sync`/`SYNC_FF`（BlackBox + 单元库） vs `SynchronizerReg`（行为级 + desiredName），建议统一。
3. **命名风格三套并存**：`i_clk/i_data/o_data`（Sync）、`clk/din/dout`（Sync2）、`srcClk/dstClk`（AsyncBus/AsyncPulse）；参数 `StageNum/Width`（PascalCase）与全组 camelCase 不一致。
4. `AsyncBus` ack 死链路、`Handshake.dstReady` 死端口、`AsyncBus.dstData` 未同步直通 —— 半成品风险点。
5. 复位语义隐式依赖 init=0（`ResetCatchAndSync` 与 `Sync2` 均依赖 `AsyncResetSynchronizerShiftReg` 默认 init 0），建议显式传参。
6. 测试：`src/test/scala/BaseCbb/async/`（AsyncBusSpec/AsyncPulseSpec/AsyncUnitsSpec/SyncSpec，共约 150 行）。
