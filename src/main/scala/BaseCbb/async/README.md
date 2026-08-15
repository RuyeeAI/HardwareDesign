# Async 异步/跨时钟域模块

## 概览

路径: `BaseCbb.async`

提供多比特同步器、脉冲跨越、握手传输、异步 FIFO 核心和格雷码计数器等跨时钟域电路。

---

## 同步器

### Sync — 多比特同步器

```scala
class Sync(StageNum: Int = 2, Width: Int = 1) extends Module
```

将 `Width` 位宽的数据从 `i_clk` 域同步输出。每比特使用 `StageNum` 级 `SYNC_FF` BlackBox 触发器链。

| 信号 | 方向 | 类型 | 说明 |
|------|------|------|------|
| `i_clk` | Input | Clock | 源时钟 |
| `i_data` | Input | UInt(Width.W) | 源数据 |
| `o_data` | Output | UInt(Width.W) | 同步后数据 |

**companion object** `Sync.apply(clk, d, stageNum)` 工厂方法。

### Sync2 — 2 级同步器

```scala
class Sync2(depth: Int = 2) extends Module
```

单比特多级同步器，`depth >= 2`。用于单比特信号跨时钟域。

| 信号 | 类型 |
|------|------|
| `clk` | Clock |
| `rst_n` | AsyncReset |
| `din` | Bool |
| `dout` | Bool |

---

## 脉冲与边沿检测

### PulseSync — 脉冲同步器

```scala
class PulseSync extends Module
```

将源时钟域的单周期脉冲同步到目标域。使用 toggle-FF + 2-ff 同步 + 边沿检测方案。

| 关键信号 | 时钟域 |
|----------|--------|
| `srcClk` / `srcRst_n` / `pulseIn` | 源域 |
| `dstClk` / `dstRst_n` / `pulseOut` | 目标域 |

### AsyncPulse — 异步脉冲跨越

```scala
class AsyncPulse extends Module
```

基于 request-acknowledge 协议的脉冲跨越。

### EdgeDetect — 边沿检测

```scala
class EdgeDetect extends Module
```

| 输出 | 说明 |
|------|------|
| `rising` | 上升沿 |
| `falling` | 下降沿 |
| `any` | 任意边沿 |

---

## 握手与数据传输

### Handshake[T] — 4 相握手

```scala
class Handshake[T <: Data](dataType: T) extends Module
```

经典 4 相握手 (req/ack)，支持任意类型跨域传输。

### AsyncBus[T] — 异步总线传输

```scala
class AsyncBus[T <: Data](gen: T) extends Module
```

基于 2 相握手的数据总线跨越。

### AsyncHandshake — 异步握手 FIFO

```scala
class AsyncHandshake(dataType: UInt, dataWidth: Int = 32) extends Module
```

基于内部 `AsyncFifoCore` 的异步 FIFO 封装，两侧均提供 `wrValid/Ready` 和 `rdValid/Ready` 握手。

---

## 复位与计数器

### AsyncRstSync — 异步复位同步释放

```scala
class AsyncRstSync extends Module
```

接收 `asyncRst` (AsyncReset)，输出 `syncRst` (AsyncReset，已同步释放)。

### GrayCounter — 格雷码计数器

```scala
class GrayCounter(width: Int = 4) extends Module
```

同时输出 `binary` 和 `gray` 形式的计数值，用于异步 FIFO 指针。

---

## 内部模块

### AsyncFifoCore — 异步 FIFO 核心

```scala
class AsyncFifoCore(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

异步 FIFO 内部核心，使用格雷码指针、2-ff 同步器进行满/空判断。被 `AsyncFifo` 和 `AsyncHandshake` 使用。

---

## ResetCatchAndSync — 复位同步器

```scala
class ResetCatchAndSync(sync: Int = 3) extends Module
```

异步复位生效，同步释放。内部使用 `AsyncResetSynchronizerShiftReg` 链。支持 PSD DFT 测试模式旁路。

| 信号 | 方向 | 类型 | 说明 |
|------|------|------|------|
| `sync_reset` | Output | Bool | 同步释放后的复位信号 |
| `psd` | Input | PSDTestMode | PSD 测试模式控制 |

**companion object** 提供 `apply(clk, rst, sync, name, psd)` 工厂方法。

---

## PSDTestMode — DFT 测试模式 Bundle

```scala
class PSDTestMode extends Bundle
```

| 信号 | 类型 | 说明 |
|------|------|------|
| `test_mode` | Bool | 测试模式使能 |
| `test_mode_reset` | Bool | 测试模式复位线 |

---

## BlockDuringReset — 复位阻塞

```scala
object BlockDuringReset
```

阻塞事务直到复位后第一个周期。使用 `Blockable` 类型类。

```scala
BlockDuringReset(data: T, stretchCycles: Int = 0): T
```

---

## Blockable — 阻塞类型类

```scala
trait Blockable[T <: Data]
```

为类型提供 `blockWhile(enable_blocking, data)` 能力。内置实例:
- `BlockableBool` — 与 `!enable_blocking` 进行 AND
- `BlockableDataCanBeValid` — 将 `.valid` 设为 false
- `BlockableDecoupled` — 冻结 Decoupled 握手
- `BlockableVec` — 对 Vec 各元素递归应用
