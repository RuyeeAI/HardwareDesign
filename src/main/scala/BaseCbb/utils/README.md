# Utils 通用工具模块

## 概览

路径: `BaseCbb.utils`

提供数据流水线、异步复位寄存器、移位队列、压缩/分散、文件 IO、注解等通用基础设施。

---

## 流水线与寄存器

### LatencyPipe — 延迟流水线

```scala
class LatencyPipe[T <: Data](typ: T, latency: Int) extends Module
```

`Decoupled` 接口数据延迟模块，使用 Queue 流水线实现。

**companion**: `LatencyPipe(in, latency, name)` → `DecoupledIO[T]`

### LatencyPipeV — Valid 接口延迟流水线

```scala
class LatencyPipeV[T <: Data](typ: T, latency: Int) extends Module
```

`Valid` 接口版本。**companion**: `LatencyPipeV(in, latency, name)` → `Valid[T]`

### RegEn — 使能寄存器

```scala
class RegEn[T <: Data](typ: T) extends Module
```

`Valid` 接口的 `RegEnable` 打拍。**companion**: `RegEn(in, name)` → `Valid[T]`

---

## 异步复位寄存器

### AsyncResetReg / AsyncResetRegVec

```scala
class AsyncResetReg(resetValue: Int = 0) extends RawModule
class AsyncResetRegVec(w: Int, init: BigInt) extends Module
```

黑盒异步复位寄存器（供 EDA 工具综合）。`AsyncResetRegVec` 使用 `SimpleRegIO(w)` 接口（`d`, `q`, `en`）。

**companion object** 提供多个 `apply()` 重载，支持单 bit 和多 bit、带/不带使能、带/不带名称。

---

## ShiftQueue — 移位队列

```scala
class ShiftQueue[T <: Data](gen: T, entries: Int, pipe: Boolean = false, flow: Boolean = false)
```

基于移位寄存器的队列，继承 `QueueIO`，额外输出 `mask: UInt(entries.W)` 显示有效槽位。

- `pipe=true`: 出队时入队可立即就绪
- `flow=true`: 有空位时 bypass

**companion**: `ShiftQueue(enq, entries, pipe, flow)` → `DecoupledIO[T]`

---

## Compress / Scatter — 数组压缩与分散

```scala
class Compress[T <: Data](gen: T, n: Int) extends Module
class Scatter[T <: Data](gen: T, n: Int) extends Module
```

`Compress`: 将 `in` 中 `valid` 有效元素压缩到 `out` 的 LSB 侧，输出 `count`（有效元素个数）。
`Scatter`: 逆操作 — 将 `in` 的前缀元素分散到 `mask` 指定的位置。

基于并行前缀和 + MuxCase 路由实现，O(n log n) 面积，O(log n) 延迟。

---

## 其他工具

| 类/对象 | 说明 |
|----------|------|
| `ShiftRegEn` | 带使能的移位寄存器，`apply(in, n, en, name)` 工厂方法 |
| `Seq2Vec` | `Seq[T]` → `Wire(Vec(...))` |
| `SubVec` | 从 Vec 中提取子向量 |
| `Convert2dArray` | 二维 `Vec[Vec[T]]` 转置 |
| `GenProcessBuilder` | 执行系统命令 |
| `ReadFile` / `WriteFile` | 文件读写 |
| `JsonTools` | JSON 序列化/反序列化 |
| `Annotated` | FIRRTL 注解（SRAM、中断）辅助方法 |
| `DecoupledHelper` | 多信号握手辅助，管理 ready/valid 使能术语 |
| `MuxT` / `MuxTLookup` | 元组多路复用（2/3/4 元素）和级联查找 |
| `ValidMux` | `ValidIO[T]` 流多路复用 |
| `Str` | 字符串/整数转 ASCII `UInt`，用于硬件调试 |
| `Split` | 从 `UInt` 提取位域到元组 |
| `Random` | 加权随机分布生成（`Random(mod)`, `oneHot(mod)`） |
| `Majority` | 多数投票逻辑（`Bool` 集合/序列/UInt） |
| `PopCountAtLeast` | 优化的 "至少 N 位为 1" 检测 |
| `MaskGen` | 字节掩码生成 (`addr_lo`, `lgSize`, `beatBytes`) |
| `MuxLiteral` / `MuxSeq` / `MuxTable` | 基于字面量的高效 Mux 查找表 |
| `ShiftRegInit` | 带 init 值和命名的移位寄存器 |
| `AbstractPipelineReg` | 流水线寄存器抽象基类（可被后端替换） |
| `AsyncResetShiftReg` | 异步复位移位寄存器数组（W 位 x D 深度） |
| `SynchronizerShiftReg` | CDC 同步器移位寄存器（无复位） |
| `AsyncResetSynchronizerShiftReg` | CDC 同步器移位寄存器（异步复位） |
| `ResetSynchronizerShiftReg` | CDC 同步器移位寄存器（推断复位） |
| `ClockCrossingReg` | 带使能的 CDC 单拍寄存器 |
| `ZCounter` | 可生成 0 宽度输出的计数器 |
| `TwoWayCounter` | 上下计数器 |
| `WideCounter` | 时钟门控宽计数器（LSB 进位门控 MSB） |
| `Timer` / `SimpleTimer` / `DynamicTimer` | 静态/动态周期倒计时定时器 |
| `ReduceOthers` | `out[i] = AND[j!=i] in[j]` 逻辑 |
| `IDPool` | 位图 ID 分配器/释放器 |
| `SeededRandom` | 可重现的 Scala 侧 PRNG（种子 42） |
| `SimpleProduct` | `Product` 的 `equals`/`hashCode`/`toString` 混入 trait |
| `Broadcaster` | 单 Decoupled 入 → N Decoupled 出广播 |
| `Repeater` | 转发输入，可在 `repeat` 时保持和重放 |
| `RipplePrefixSum` / `DensePrefixSum` / `SparsePrefixSum` | 并行前缀和网络 |
| `DelayQueue` | 按可编程周期数延迟元素的队列 |
| `ReorderQueue` | 乱序完成缓冲区（CAM 或直接索引） |
| `HeterogeneousBag[T]` | 异构数据元素的 Record（整数索引） |
| `RecordMap[T]` | 基于 `ListMap` 的类型安全 Record |


---

## 注解类型

| 注解 | 说明 |
|------|------|
| `SRAMAnnotation` | SRAM 元数据（地址宽度、深度、描述等） |
| `InterruptsPortAnnotation` | 中断端口元数据 |
| `GlobalConstantsAnnotation` | 全局常量（如 xLen） |
| `ParamsAnnotation` | 参数元数据 |
