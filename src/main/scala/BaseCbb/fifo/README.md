# FIFO 模块

## 概览

路径: `BaseCbb.fifo`

提供同步/异步 FIFO，memory 接口均置于模块外部，通过 `TpMemoryPort` / `SpMemoryPort` 连接外部 SRAM。

---

## 同步 FIFO

### SyncFifo — 标准同步 FIFO

```scala
class SyncFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

深度 = `2^addrWidth`。`dout` 为寄存器输出（1 拍延迟）。

| 信号 | 方向 | 类型 | 说明 |
|------|------|------|------|
| `mem` | Flipped | TpMemoryPort | 外部双口 SRAM 接口 |
| `clk` | Input | Clock | 时钟 |
| `rst_n` | Input | AsyncReset | 异步复位 |
| `wrEn` / `din` | Input | — | 写使能/数据 |
| `rdEn` | Input | Bool | 读使能 |
| `dout` | Output | UInt | 读数据（寄存器输出） |
| `empty` / `full` | Output | Bool | 空/满标志 |
| `level` | Output | UInt | 当前数据量 |

### SyncZeroLatencyFifo — 零延迟同步 FIFO

```scala
class SyncZeroLatencyFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

接口同 `SyncFifo`，但 `dout` 为纯组合路径输出。

### RegisterBasedFifo — 寄存器堆 FIFO

```scala
class RegisterBasedFifo(dataWidth: Int = 32, depth: Int = 8) extends Module
```

使用 `Reg(Vec(depth, ...))` 实现，**无需外部 SRAM**。`depth <= 32`。

### DualSinglePortRamFifo — 双 Bank FIFO

```scala
class DualSinglePortRamFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

用两个单口 SRAM (bank0/bank1) 模拟双口 SRAM。MSB 地址位选择 bank。

| 额外信号 | 类型 | 说明 |
|----------|------|------|
| `memBank0` | Flipped(SpMemoryPort) | Bank0 接口 |
| `memBank1` | Flipped(SpMemoryPort) | Bank1 接口 |

---

## 异步 FIFO

### AsyncFifo — 标准异步 FIFO

```scala
class AsyncFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

写侧和读侧使用独立时钟和复位。

| 写侧 | 读侧 |
|------|------|
| `wrClk` / `wrRst_n` | `rdClk` / `rdRst_n` |
| `wrEn` / `din` | `rdEn` / `dout` |
| `full` / `wrLevel` | `empty` / `rdLevel` |

### AsyncZeroLatencyFifo — 零延迟异步 FIFO

```scala
class AsyncZeroLatencyFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

接口同 `AsyncFifo`（无 `wrLevel`/`rdLevel`），`dout` 为组合输出。
