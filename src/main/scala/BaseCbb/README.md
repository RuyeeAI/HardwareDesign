# BaseCbb Hardware Design Library

BaseCbb 是一个基于 Chisel 的硬件设计基础库，提供常用电路模块的 RTL 实现，包括：基本门级单元、时序单元、跨时钟域单元、仲裁器、存储器/FIFO、算术单元、寄存器块 DSL 和 Clos 网络。

---

## 包结构

| 路径 | 内容 |
|------|------|
| `basic/` | 基本门级单元（AND/OR/MUX/DFF 等） |
| `sequential/` | 时序单元（寄存器、计数器、时钟分频器、FSM） |
| `async/` | 跨时钟域单元（同步器、脉冲跨越、异步总线） |
| `arbiter/` | 仲裁器（RR、WRR、iSlip） |
| `memory/` | 存储模型、SRAM 封装、Bitmap 分配器、链表队列 |
| `fifo/` | 同步/异步 FIFO（含外部 memory 端口） |
| `utils/` | 基础构件（LatencyPipe、ShiftQueue、Compress、AsyncResetReg） |
| `arithmetic/` | 算术单元（加法器、乘法器、移位器、比较器） |
| `RegCbb/` | 寄存器块 DSL、AXI 接口、地址分配器 |
| `Clos/` | Benes/Clos 网络（置换网络） |
| `Area/` | 面积估算模型 |

---

## 1. basic/ — 基本门级单元

路径：`BaseCbb.basic`

所有模块均无内部状态（纯组合逻辑），直接映射到标准工艺库单元。

### 基本门

| 类 | 说明 |
|----|------|
| `Inv` | 反相器 |
| `Buf` | 缓冲器 |
| `And2` / `And3` | 2/3 输入与门 |
| `Nand2` / `Nand3` | 2/3 输入与非门 |
| `Or2` | 2 输入或门 |
| `Nor2` / `Nor3` | 2/3 输入或非门 |
| `Xor2` / `Xnor2` | 2 输入异或/同或门 |

### 组合选择

| 类 | 说明 |
|----|------|
| `Mux2` | 2 选 1 多路复用器 |
| `Mux2N[T]` | 参数化类型 2 选 1 多路复用器 |
| `Dec2` | 2-4 译码器（one-hot 输出） |
| `Dec3` | 3-8 译码器 |

### 时序单元

| 类 | 说明 |
|----|------|
| `DLatch` | D 锁存器（低使能） |
| `DFF` | D 触发器（无复位） |
| `DFFAsyncRst` | 异步复位 D 触发器 |
| `DFFSyncRst` | 同步复位 D 触发器 |

### 算术/特殊单元

| 类 | 说明 |
|----|------|
| `HalfAdd` | 半加器（a + b → sum, cout） |
| `FullAdd` | 全加器（a + b + cin → sum, cout） |
| `SRLatch` | SR 锁存器 |
| `ClockGating` | 时钟门控（避免时钟树毛刺） |
| `AOI22` | AND-OR-Invert 单元（(a∧b)∨(c∧d) 取反） |
| `AOI32` | AND-OR-Invert 单元（(a∧b∧c)∨(d∧e∧f) 取反） |

**ClockGating 原理：**
```scala
latchEn := RegEnable(io.en, false.B, !io.clk)  // clk 下降沿锁存使能
gatedClk := io.clk & latchEn                     // 避免毛刺
```

---

## 2. sequential/ — 时序电路单元

路径：`BaseCbb.sequential`

### Register — N 位寄存器

```scala
class Register(width: Int = 32) extends Module
```

| 信号 | 方向 | 类型 | 说明 |
|------|------|------|------|
| `clk` | Input | Clock | 时钟 |
| `rst_n` | Input | AsyncReset | 异步低有效复位 |
| `din` | Input | UInt(width.W) | 数据输入 |
| `wen` | Input | Bool | 写使能 |
| `dout` | Output | UInt(width.W) | 数据输出 |

### RegFile1R1W / RegFile2R1W — 寄存器文件

| 类 | 说明 |
|----|------|
| `RegFile1R1W` | 1 读 1 写端口寄存器文件 |
| `RegFile2R1W` | 2 读 1 写端口寄存器文件 |

### 计数器

| 类 | 说明 |
|----|------|
| `UpCounter(width)` | 递增计数器，上溢时 `carry=1` |
| `ModNCounter(mod)` | 模 N 计数器，上溢后回到 0，输出 `overflow` |

### 时钟分频器

| 类 | 说明 |
|----|------|
| `ClkDiv2` | 2 分频（占空比 50%） |
| `ClkDivOdd(div)` | 奇数分频（占空比 50%） |
| `ClkDiv(div)` | 通用整数分频（非 50%） |
| `ClockDivider2` | 2 分频 BlackBox（`Clock` 输入/输出） |
| `ClockDivider3` | 3 分频 BlackBox |
| `Pow2ClockDivider(pow2)` | 2^pow2 分频（链式 ClockDivider2） |

**ClkDivOdd 设计：** 使用 `ClkPos`（上沿计数）和 `ClkNeg`（下沿计数）两个计数器组合，实现任意奇数分频的 50% 占空比。

### FsmTemplate — FSM 模板

```scala
class FsmTemplate(stateNum: Int = 4) extends Module
```

状态机模板，输出 `idle` / `busy` / `done` 三个状态标志。

---

## 3. async/ — 跨时钟域电路

路径：`BaseCbb.async`

### Sync — 多比特同步器

```scala
class Sync(StageNum: Int = 3, Width: Int = 1) extends Module
```

将多比特数据从 `i_clk` 时钟域同步到输出。每比特使用独立的 `StageNum` 级触发器链（基于 `SYNC_FF` BlackBox）。

| 信号 | 方向 | 说明 |
|------|------|------|
| `i_clk` | Input | 源时钟 |
| `i_data` | Input | UInt(Width) | 源数据 |
| `o_data` | Output | UInt(Width) | 同步后数据 |

### Sync2 — 2 级同步器

```scala
class Sync2(depth: Int = 2) extends Module
```

单比特 2-级同步器，可配置深度（默认 2），用于单比特信号跨越时钟域。

### PulseSync — 脉冲同步器

将源时钟域的单周期脉冲同步到目标时钟域。使用"电平-toggle-检测"方式：

```
pulse_a (源域) → toggle FF → 2-ff 同步 → 边沿检测 → pulse_b (目标域)
```

### EdgeDetect — 边沿检测

```scala
class EdgeDetect extends Module
```

检测输入信号的上升沿、下降沿和任意边沿。

### AsyncRstSync — 异步复位同步释放

将异步产生的复位信号同步释放到目标时钟域，避免异步复位亚稳态。

### Handshake[T] — 4 相握手

```scala
class Handshake[T <: Data](dataType: T) extends Module
```

经典 4 相握手机制（req/ack），支持任意数据类型跨域传输。

| 信号 | 方向 | 说明 |
|------|------|------|
| `srcValid` | Input | 源侧数据有效 |
| `srcReady` | Output | 源侧可以发送 |
| `srcData` | Input | 源侧数据 |
| `dstValid` | Output | 目标侧数据有效 |
| `dstReady` | Input | 目标侧可以接收 |
| `dstData` | Output | 目标侧数据 |

### GrayCounter — 格雷码计数器

```scala
class GrayCounter(width: Int = 4) extends Module
```

同时输出二进制和格雷码形式的计数器，用于异步 FIFO 指针。

### AsyncHandshake — 异步数据握手

基于内部 `AsyncFifoCore` 的异步 FIFO 封装，同时提供 `wrValid/Ready` 和 `rdValid/Ready` 握手接口。

### AsyncPulse — 脉冲跨越

```scala
class AsyncPulse extends Module
```

使用 request-acknowledge 协议将脉冲从 `clk_a` 域跨越到 `clk_b` 域。

### AsyncBus[T] — 异步总线

```scala
class AsyncBus[T <: Data](gen: T) extends Module
```

基于 2 相握手的通用数据总线跨越模块。

### ResetCatchAndSync — 复位同步器

```scala
class ResetCatchAndSync(sync: Int = 3) extends Module
```

异步复位生效、同步释放，支持 PSD DFT 测试模式旁路。

### PSDTestMode — DFT 测试模式 Bundle

`test_mode` / `test_mode_reset` 信号供复位和 DFT scan 控制。

### BlockDuringReset — 复位阻塞

```scala
object BlockDuringReset
def apply[T <: Data : Blockable](data: T, stretchCycles: Int = 0): T
```

阻塞传输直到复位后第一个周期。

### Blockable — 阻塞类型类

为 `Bool`、`DataCanBeValid`、`DecoupledIO`、`Vec` 提供 `blockWhile(enable_blocking, data)` 能力。

---

## 4. arbiter/ — 仲裁器

路径：`BaseCbb.arbiter`

### RR — 轮询仲裁器

```scala
class RR(ClientNum: Int) extends Module
```

固定优先级轮询仲裁器。每次 grant 后指针旋转到下一位，下次优先响应下一个客户。

| 信号 | 方向 | 类型 | 说明 |
|------|------|------|------|
| `ready` | Input | UInt(ClientNum) | 各客户请求信号 |
| `grant` | Output | UInt(ClientNum) | 独热 grant 授予信号 |
| `enable` | Input | Bool | 使能 |

### WRR — 加权轮询仲裁器

```scala
class WRR(ClientNum: Int, WtWidth: Int) extends Module
```

支持权重的轮询仲裁。每个客户的权重由 `weight` 向量指定，当权重用尽时切换到下一个客户。

| 额外信号 | 方向 | 说明 |
|------|------|------|
| `weight` | Input | Vec(ClientNum, UInt(WtWidth)) | 各客户权重 |

### iSlipLogic / RegulariSlip — iSlip 调度器

用于 crossbar 交换芯片的并行迭代调度算法：

1. **Stage 1（目标侧）**：每个输出从 `dst_ptr` 开始轮询，选择第一个有请求的输入
2. **Stage 2（输入侧）**：每个输入从 `src_ptr` 开始轮询，选择第一个有请求的输出

与 RR 的区别：iSlip 在每次 grant 后将指针推进到已被服务的下一位，实现最大匹配。

### HellaLockingArbiter — 带锁轮询仲裁器

```scala
abstract class HellaLockingArbiter[T <: Data](typ: T, arbN: Int, rr: Boolean = false)
```

泛型锁定仲裁器基类。可选 RR 模式，锁定时保持选中的客户端。

- **HellaPeekingArbiter** — 通过窥探 `canUnlock(data)` 函数决定解锁时机
- **HellaCountingArbiter** — 锁定固定 `count` 个事务后释放，适用于多拍 burst 传输

---

## 5. memory/ — 存储模型与分配器

路径：`BaseCbb.memory`

### 5.1 Memory.scala — 存储配置与封装

#### `Memory` — case class（配置对象）

```scala
case class Memory(
  name:           String,
  dataType:       Data,
  depth:          Int,
  memoryType:     String = "1RW",   // "1RW" | "2RW"
  flopIn:         Boolean = false, // 输入打拍
  flopOut:        Boolean = true,  // 输出打拍
  protect:        String = "ECC",   // "ECC" | "Parity" | "none"
  protectWidThre: Int = 320        // ECC 分段阈值
)
```

**计算属性：**

| 属性 | 说明 |
|------|------|
| `dataWidth` | 实际位宽（含 ECC/Parity 开销） |
| `latency` | 总延迟（flopIn+flopOut+1） |
| `addrWidth` | 地址位宽 `log2Ceil(depth)` |

**ECC 位宽计算：** `eccWidth(n)` 找到最小 k 使 `2^k ≥ n + k + 1`

#### 接口类

| 类 | 说明 |
|----|------|
| `SpMemoryPort` | 单口存储接口（we/re/addr/wdata/rdata） |
| `TpMemoryPort` | 双口存储接口（we/re/waddr/raddr/wdata/rdata） |
| `SpMemoryBB` | 单口 SRAM BlackBox（物理实现） |
| `TpMemoryBB` | 双口 SRAM BlackBox（物理实现） |
| `SimMemory` | 仿真模型（基于 Vec 寄存器堆） |

#### 模式切换

```scala
class SpMemoryWrap(mem: Memory) extends MemoryWrap
class TpMemoryWrap(mem: Memory) extends MemoryWrap
```

通过覆盖 `MemoryWrap.MEM_TYPE`（`"SIMULATION"` 或其他）切换仿真/物理实现。

---

### 5.2 Bitmap — 位图资源分配器

```scala
class Bitmap(RscNum: Int) extends Module
```

用一位标识一个资源是否已被占用，支持快速的空闲资源分配。

| 信号 | 方向 | 说明 |
|------|------|------|
| `req_vld` | Input | 分配请求有效 |
| `req_ptr` | Output | 分配的资源指针（PriorityEncoder 选择最低空闲位） |
| `ret_vld` | Input | 归还请求有效 |
| `ret_ptr` | Input | 归还的资源的指针 |
| `empty` | Output | 无可用资源（所有位被占用） |
| `full` | Output | 所有资源已归还（bitmap 全 0） |

**原理：**
- `req_ptr = PriorityEncoder(bitmap)` — 选择最低位空闲资源
- `empty = (bitmap === Fill(RscNum, 1))`
- `full = (bitmap === Fill(RscNum, 0))`

---

### 5.3 BitmapCacheMem — 带缓存的位图分配器

```scala
class BitmapCacheMem(n: Int, cacheSize: Int = 64, memLatency: Int = 1) extends Module
```

将 n 个 bit 位存储在外部 SRAM 中，模块内部缓存一行（`cacheSize` bits），减少 SRAM 访存次数。

**参数：**

| 参数 | 说明 |
|------|------|
| `n` | 总资源数（bit 数），必须被 `cacheSize` 整除 |
| `cacheSize` | 缓存行大小（2 的幂） |
| `memLatency` | SRAM 读延迟周期数 |

**接口：**

| 信号 | 方向 | 说明 |
|------|------|------|
| `mem` | TpMemoryPort | 外部 SRAM 接口（`log2M` 地址，`cacheSize` 位宽） |
| `alloc_req` | Input | 分配请求 |
| `alloc_ptr` | Output | 分配得到的 n 位指针 |
| `alloc_valid` | Output | 分配有效信号 |
| `free_req` | Input | 释放请求 |
| `free_ptr` | Input | 释放的 n 位指针 |
| `init` | Input | 初始化（清零 SRAM） |
| `empty` | Output | 无可用资源 |
| `full` | Output | 所有资源均已分配 |
| `freeCnt` | Output | 当前可用资源数量 |

**内部结构：**
- `M = n / cacheSize` 行，每行 `cacheSize` bits
- 缓存一行：寄存器 `cacheData(Vec(cacheSize,Bool))` + `cacheTag(log2M)` + `cacheValid`
- `cacheFreeCnt = PopCount(cacheData.map(!_))` — 当前行空闲位数
- `firstFreeInCache = PriorityEncoder(cacheData.map(!_))` — 第一个空闲位

**状态机：**

| 状态 | 说明 |
|------|------|
| `sIdle` | 接受请求。alloc 命中缓存 → 同拍返回；未命中 → 进入 sRead |
| `sRead` | 流水线读 SRAM。每 `memLatency` 周期处理一行数据：命中空闲位则分配，否则换下一行 |
| `sWrite` | 将脏行写回 SRAM（free miss 时触发） |
| `sInit` | 初始化：逐行清零 SRAM |

**alloc 路径时序（memLatency=1）：**

| 场景 | 延迟 |
|------|------|
| Cache 命中 | **1 拍**（组合路径） |
| Cache 未命中，第 i 行有空闲 | **i+1 拍** |
| 所有 M 行全满（最坏） | **M 拍**（流水线全速运转） |

---

### 5.4 Linklist — 链表队列

#### SubLinklist — 单级链表

```scala
class SubLinklist(RamLat: Int, RscNum: Int, PtrW: Int) extends Module
```

单级链表，用 `ShiftRegister` 处理 RAM 读延迟：

```scala
link_mem_rdata_vld = ShiftRegister(io.ll_mem_intf.re, RamLat, false.B, true.B)
head_ptr_mux = Mux(link_mem_rdata_vld, io.ll_mem_intf.rdata, head_ptr)
```

#### LinkList — 多级并行链表

```scala
class LinkList(RamLat: Int, RscNum: Int, PtrW: Int) extends Module
```

- `SubLlNum = RamLat` 个子链表并行工作
- `Counter` 轮询分配 enq/deq 序列到不同子链表
- RAM 接口通过 `reduceTree(_|_)` 合并（任意时刻最多一个子链表访问 RAM）

#### VoqLinkList — VOQ 链表

```scala
class VoqLinkList(QueueNum: Int, RamLat: Int, RscNum: Int, PtrW: Int) extends Module
```

针对 Virtual Output Queue 场景：每个 VOQ 独立队列，支持多队列并发出队。

- `SubLlNum = RamLat * QueueNum` 个子链表
- `deqSel` / `enqSel` 根据队列 ID 和序列号选择对应子链表

---

## 6. fifo/ — FIFO 队列

路径：`BaseCbb.fifo`

所有 FIFO 的 memory 接口均**移至模块外部**，通过 `TpMemoryPort` 连接外部 SRAM。

### SyncFifo — 同步 FIFO

```scala
class SyncFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

| 参数 | 说明 |
|------|------|
| `dataWidth` | 数据位宽 |
| `addrWidth` | 地址位宽，深度 = `2^addrWidth` |

| 信号 | 方向 | 说明 |
|------|------|------|
| `mem` | TpMemoryPort | 外部双口 SRAM 接口 |
| `clk` | Input | 时钟 |
| `rst_n` | Input | 异步低有效复位 |
| `wrEn` | Input | 写使能 |
| `din` | Input | 写数据 |
| `rdEn` | Input | 读使能 |
| `dout` | Output | 读数据（寄存器输出，1 拍延迟） |
| `empty` | Output | 空标志 |
| `full` | Output | 满标志 |
| `level` | Output | 当前数据数量 |

**使用示例：**
```scala
val sram  = Module(new TpMemoryBB(depth, dataWidth))
val fifo  = Module(new SyncFifo(dataWidth, addrWidth))
sram.io  <> fifo.io.mem
```

---

### SyncZeroLatencyFifo — 零延迟同步 FIFO

```scala
class SyncZeroLatencyFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

与 `SyncFifo` 接口相同，但 `dout` 为**纯组合路径**（`mem.rdata` 直接输出），读地址为 `nextRdAddr`（`rdPtr + 1`，组合）。

---

### RegisterBasedFifo — 寄存器堆 FIFO

```scala
class RegisterBasedFifo(dataWidth: Int = 32, depth: Int = 8) extends Module
```

使用 `Reg(Vec(depth, UInt(dataWidth.W)))` 实现，**无需外部 SRAM**，适合深度 ≤ 32 的小 FIFO。

---

### DualSinglePortRamFifo — 双 Bank Ping-Pong FIFO

```scala
class DualSinglePortRamFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

用**两个单口 SRAM**（bank0/bank1）模拟双口 SRAM：

- `wrBank = wrPtr(addrWidth-1)` — MSB 选择写 bank
- `rdBank = rdPtr(addrWidth-1)` — MSB 选择读 bank
- 各 bank 使用 `addrWidth-1` 位地址（深度减半）

| 信号 | 类型 | 说明 |
|------|------|------|
| `memBank0` | SpMemoryPort | Bank0 单口 SRAM 接口（深度 = `2^(addrWidth-1)`） |
| `memBank1` | SpMemoryPort | Bank1 单口 SRAM 接口（深度 = `2^(addrWidth-1)`） |

---

### AsyncFifo — 异步 FIFO

```scala
class AsyncFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

| 信号 | 方向 | 说明 |
|------|------|------|
| `mem` | TpMemoryPort | 外部双口 SRAM 接口 |
| `wrClk` / `wrRst_n` | Input | 写侧时钟/复位 |
| `wrEn` / `din` | Input | 写使能/写数据 |
| `full` | Output | 写侧满标志 |
| `wrLevel` | Output | 写侧数据计数 |
| `rdClk` / `rdRst_n` | Input | 读侧时钟/复位 |
| `rdEn` | Input | 读使能 |
| `dout` | Output | 读数据（寄存器输出） |
| `empty` | Output | 读侧空标志 |
| `rdLevel` | Output | 读侧数据计数 |

**设计：**
- 格雷码指针（`addrWidth+1` 位）跨时钟域同步
- 满判断：`wrPtrGray` 和 `rdGraySync2` 的 MSB、次MSB 不同，低位相同
- 空判断：读写指针格雷码相等
- 2-ff 同步器将格雷码指针同步到对侧时钟域

---

### AsyncZeroLatencyFifo — 零延迟异步 FIFO

```scala
class AsyncZeroLatencyFifo(dataWidth: Int = 32, addrWidth: Int = 4) extends Module
```

与 `AsyncFifo` 接口相同（无 `wrLevel`/`rdLevel`），但 `dout` 为零延迟组合输出。

---

## 7. arithmetic/ — 算术单元

路径：`BaseCbb.arithmetic`

### 加法器

| 类 | 说明 |
|----|------|
| `RippleCarryAdder(width)` | 逐位进位 ripple 加法器（N 个 FullAdd 链式连接） |
| `CarrySelectAdder(width, blockSize)` | 进位选择加法器（分段并行计算 carry0/carry1 后选择） |
| `Subtractor(width)` | 减法器（`a + ~b + 1`） |
| `AddSub(width)` | 加法/减法可配置（`b ^ sub` 选择） |

### 比较器

```scala
class Comparator(width: Int = 32) extends Module
```

| 信号 | 方向 | 说明 |
|------|------|------|
| `a` / `b` | Input | UInt(width.W) | 比较操作数 |
| `eq` | Output | a == b |
| `gt` | Output | a > b |
| `lt` | Output | a < b |

### 乘法器

```scala
class Multipler(widthA: Int = 16, widthB: Int = 16) extends Module
```

输出宽度 = `widthA + widthB`。

### 移位器

| 类 | 说明 |
|----|------|
| `LeftShifter(width)` | 左移位器（`din << shamt`） |
| `RightShifter(width, arithmetic)` | 右移位器，支持逻辑/算术移位 |

---

## 8. utils/ — 通用工具

路径：`BaseCbb.utils`

### LatencyPipe — 延迟流水线

```scala
class LatencyPipe[T <: Data](typ: T, latency: Int) extends Module
```

数据延迟模块，使用 Queue 流水线实现任意周期延迟。

### RegEn — 使能寄存器

```scala
class RegEn[T <: Data](typ: T) extends Module
```

带使能的流水线寄存器，`Valid` 接口通过：`in(Valid) → RegEnable → out(Valid)`。

### ShiftQueue — 移位队列

```scala
class ShiftQueue[T <: Data](gen: T, entries: Int, pipe: Boolean = false, flow: Boolean = false)
```

基于移位寄存器的队列，支持 `pipe`（出队时入队可立即就绪）和 `flow`（有空位时 bypass）模式。输出 `mask` 显示哪些槽位有效。

### AsyncResetReg — 异步复位寄存器

```scala
class AsyncResetReg(resetValue: Int = 0) extends Module
class AsyncResetRegVec(w: Int, init: BigInt) extends Module
```

黑盒异步复位寄存器（供 EDA 工具综合）。companion object 提供 `apply()` 工厂方法。

### ShiftRegEn — 带使能的移位寄存器

```scala
object ShiftRegEn {
  def apply[T <: Data](in: T, n: Int, en: Bool, name: String = ""): T
}
```

用 `foldRight` 实现：`val regs = (0 until n).foldRight(in)((i, last) => RegEnable(last, en))`

### Compress / Scatter — 数组压缩与分散

```scala
class Compress[T <: Data](gen: T, n: Int) extends Module
class Scatter[T <: Data](gen: T, n: Int) extends Module
```

`Compress`: 将 `in` 中 `valid` 有效元素压缩到 `out` 的 LSB 侧，`count` 输出有效元素数量。
`Scatter`: 逆操作 — 将 `in` 的前缀元素分散到 `mask` 指定的位置。

基于并行前缀和 + MuxCase 路由，O(n log n) 面积，O(log n) 延迟。

### 其他工具

| 类/对象 | 说明 |
|----------|------|
| `GenProcessBuilder` | 执行系统命令 |
| `Seq2Vec` | `Seq[T]` → `Vec[T]` |
| `SubVec` | 从 Vec 中提取子向量 |
| `Convert2dArray` | 二维数组转置 |
| `DecoupledHelper` | 多信号 Decoupled 握手辅助（条件 fire） |
| `MuxT` / `MuxTLookup` | 元组多路复用（2/3/4 元素）和级联查找 |
| `ValidMux` | `ValidIO[T]` 流多路复用 |
| `Str` | 字符串/整数转 ASCII `UInt`（硬件调试打印） |
| `Split` | `UInt` 位域提取到元组 |
| `Random` | 加权随机分布生成器 |
| `Majority` | 多数投票逻辑 |
| `PopCountAtLeast` | 优化的 popcount >= N 检测 |
| `MaskGen` | 字节掩码生成 (addr_lo, lgSize, beatBytes) |
| `MuxLiteral` / `MuxSeq` / `MuxTable` | 基于字面量的高效 Mux 查找表 |
| `ShiftRegInit` | 带 init 值和命名的移位寄存器 |
| `AbstractPipelineReg` | 流水线寄存器抽象（可被后端替换） |
| `AsyncResetShiftReg` | 异步复位移位寄存器阵列 |
| `SynchronizerShiftReg` | CDC 同步器移位寄存器 |
| `AsyncResetSynchronizerShiftReg` | 异步复位 CDC 同步器 |
| `ResetSynchronizerShiftReg` | 推断复位 CDC 同步器 |
| `ClockCrossingReg` | 带使能的 CDC 单拍寄存器 |
| `ZCounter` | 可变宽度计数器（count-to-1 时输出 0 宽度） |
| `TwoWayCounter` | 上下双向计数器 |
| `WideCounter` | 时钟门控宽计数器 |
| `Timer` / `SimpleTimer` / `DynamicTimer` | 定时器（多路/单路/动态周期） |
| `ReduceOthers` | out[i] = AND[j!=i] in[j] 逻辑 |
| `IDPool` | 位图 ID 分配/释放器 |
| `SeededRandom` | 可重现 Scala PRNG |
| `SimpleProduct` | case-class 相等/hash/toString 混入 trait |
| `Broadcaster` | 1 个 Decoupled 入 → N 个 Decoupled 出 |
| `Repeater` | 可重复/重放的 Decoupled 中继器 |
| `RipplePrefixSum` / `DensePrefixSum` / `SparsePrefixSum` | 并行前缀和网络 |
| `DelayQueue` | 可编程周期延迟的元素队列 |
| `ReorderQueue` | 乱序完成缓冲区 |
| `HeterogeneousBag[T]` | 异构数据 Record（整数索引访问） |
| `RecordMap[T]` | 基于 `ListMap` 的类型安全 Record |

---

## 9. RegCbb/ — 寄存器块框架

路径：`BaseCbb.RegCbb`

### 9.1 AxiInterfaces — AXI 总线接口定义

#### AxiLiteBusIO

```scala
class AxiLiteBusIO(addrWidth: Int, dataWidth: Int) extends Bundle
```

AXI4-Lite 接口，包含 5 个通道：

| 通道 | 信号 |
|------|------|
| 写地址 | `aw_valid/aw_ready/aw_addr/aw_prot` |
| 写数据 | `w_valid/w_ready/w_data/w_strb` |
| 写响应 | `b_valid/b_ready/b_resp` |
| 读地址 | `ar_valid/ar_ready/ar_addr/ar_prot` |
| 读数据 | `r_valid/r_ready/r_data/r_resp` |

#### AxiBusIO

```scala
class AxiBusIO(addrWidth: Int, dataWidth: Int, idWidth: Int = 4) extends Bundle
```

完整 AXI4 接口（支持 Burst），额外包含：`aw_len/aw_size/aw_burst/aw_lock/aw_cache/aw_qos`、`w_last`、`b_id`、`ar_len/ar_size/ar_burst/ar_lock/ar_cache/ar_qos`、`r_id/r_last`。

#### 常量对象

| 对象 | 值 |
|------|----|
| `AxiBurstType.FIXED` | `0b00` |
| `AxiBurstType.INCR` | `0b01` |
| `AxiBurstType.WRAP` | `0b10` |
| `AxiResp.OKAY` | `0b00` |
| `AxiResp.EXOK` | `0b01` |
| `AxiResp.SLVERR` | `0b10` |
| `AxiResp.DECERR` | `0b11` |

---

### 9.2 RegType — 寄存器访问类型

每种寄存器类型均包含两个接口：
- **Decoder 接口 (dec)**：连接总线协议层，处理地址解码和总线握手
- **Core 接口 (core)**：连接用户逻辑，提供寄存器访问信号

#### 9.2.1 Decoder 接口 (dec)

```scala
class dec_if[T <: Data](gen: T = UInt(32.W)) extends Bundle {
  val in = new dec_in(gen)    // 输入：wr, wdata, rd
  val out = new dec_out(gen)   // 输出：rdata
}

class dec_in[T <: Data](gen: T) extends Bundle {
  val wr = Input(Bool())      // 写使能
  val wdata = Input(gen)       // 写数据
  val rd = Input(Bool())      // 读使能
}

class dec_out[T <: Data](gen: T) extends Bundle {
  val rdata = Output(gen)     // 读数据
}
```

#### 9.2.2 寄存器类型详解

| 类型 | 名称 | 行为 | Core 接口信号 | 说明 |
|------|------|------|-------------|------|
| **RW** | 读写寄存器 | 直接写入 | `wrEn`, `wrData` | 软件可读写，最常用类型 |
| **RO** | 只读寄存器 | 用户逻辑驱动 | `wrData` (Input, 用户驱动) | 软件只读，数据由硬件设置 |
| **WO** | 只写寄存器 | 直接写入 | `wrEn`, `wrData` | 软件只写，写入后返回 0 |
| **RC** | 读清寄存器 | 读后清零 | `rdData`, `rdEn` | 读操作自动将寄存器清零 |
| **RS** | 读置寄存器 | 读后置位 | `rdData`, `rdEn` | 读操作自动将寄存器全置 1 |
| **W1C** | 写1清零 | 写入的 1 位清零对应位 | `wrEn`, `wrData` | 常用于清除中断标志 |
| **W1S** | 写1置位 | 写入的 1 位置位对应位 | `wrEn`, `wrData` | 常用于置位控制标志 |
| **W1T** | 写1翻转 | 写入的 1 位翻转对应位 | `wrEn`, `wrData` | 常用于 Toggle 型状态标志 |

#### 9.2.3 Core 接口信号定义

```scala
// RW / WO / W1C / W1S / W1T 使用
class rw_core_if(info: RegInfo) extends Bundle {
  val wrEn = Output(Bool())
  val wrData = Output(info.DataType)
}

// RO 使用（用户逻辑驱动数据到总线）
class ro_core_if(info: RegInfo) extends Bundle {
  val wrData = Flipped(Output(info.DataType))  // Input，用户驱动
}

// RC / RS 使用
class rc_core_if(info: RegInfo) extends Bundle {
  val rdData = Output(info.DataType)
  val rdEn = Output(Bool())  // 读使能脉冲
}
```

#### 9.2.4 各类型详细行为

**RW (Read-Write):**
- 写入：`when(io.dec.in.wr) { dataReg := io.dec.in.wdata }`
- 读出：`io.dec.out.rdata := dataReg`
- 软件可读可写，用户逻辑通过 `wrEn` 和 `wrData` 捕获写入

**RO (Read-Only):**
- 数据由用户逻辑通过 `core.wrData` 驱动
- 总线读返回用户驱动值
- 常用于报告硬件状态

**WO (Write-Only):**
- 写入行为同 RW
- 读返回 0（软件读不到有效数据）
- 用户逻辑通过 `wrEn` 和 `wrData` 捕获写入

**RC (Read-to-Clear):**
- 读操作后寄存器自动清零：`when(io.dec.in.rd) { dataReg := 0 }`
- `rdEn` 脉冲信号通知用户逻辑发生读操作
- 常用于中断状态寄存器

**RS (Read-to-Set):**
- 读操作后寄存器全置 1：`when(io.dec.in.rd) { dataReg := ~0 }`
- `rdEn` 脉冲信号通知用户逻辑发生读操作
- 常用于记录事件触发（首次读取记录）

**W1C (Write-1-to-Clear):**
- 写入值的 1 位清零对应位：`dataReg := dataReg & ~io.dec.in.wdata`
- 常用于清除单个标志位

**W1S (Write-1-to-Set):**
- 写入值的 1 位置位对应位：`dataReg := dataReg | io.dec.in.wdata`
- 常用于置位单个控制位

**W1T (Write-1-to-Toggle):**
- 写入值的 1 位翻转对应位：`dataReg := dataReg ^ io.dec.in.wdata`
- 常用于 Toggle 型状态变化

---

### 9.3 DSL — 寄存器块领域特定语言

#### RegFieldDsl — 字段级 DSL

```scala
RegField("name", width) { b =>
  b.named("field_name").rw().reset(0).desc("description")
  b.field(RegField("bit", 1, AccessType.RW, 0, ""))
}
```

**AccessType：** `RO` / `WO` / `RW` / `RC` / `RS`

**WriteAction：** `Normal` / `OneToClear` / `OneToSet` / `OneToToggle` / `ClearOnRead`

#### MemoryDef — 存储器定义

```scala
MemoryDef.sp("fifo", depth=1024, dataWidth=64, baseAddress=0x1000)
MemoryDef.tp("sram", depth=256, dataWidth=32)
```

支持 `sp`（单端口）和 `tp`（双端口）。

#### AddressAllocator — 地址分配

自动将寄存器和存储器映射到地址空间，支持 C header 生成。

#### RegisterBlockDsl — 寄存器块 DSL

```scala
RegBlock("myBlock", 0x4000_0000) { b =>
  b.reg("ctrl") {
    b.field(RegField.rw("en", 1))
    b.field(RegField.rw("mode", 2))
  }
  b.mem("fifo") { mb =>
    mb.sp().depth(1024).dataWidth(64)
  }
}
```

#### RegisterIRGenerator — JSON IR 生成

```scala
RegisterIRGenerator.generate(map)  // 生成 RegBlockIR
toJson(ir)                          // 格式化 JSON
toCHeader(ir)                      // 生成 C 头文件
```

#### RegisterFileGenerator — RTL 生成

从地址映射生成实际的寄存器堆实现：
- `Reg` 存储每个寄存器
- `Mem` 存储每个存储器
- 地址解码器
- 写动作处理（W1C/W1S/W1T/RC）

#### GenRegBlock — 基于 GenBundle 的寄存器块

从 `GenBundle` 结构自动推断寄存器访问类型：

```scala
class MyRegs extends GenBundle {
  val ctrl   = UInt(32.W)   // 默认 RW
  val status_ro = UInt(16.W) // _ro 后缀 → RO
  val flag_w1c = UInt(8.W)   // _w1c 后缀 → W1C
}
```

`AxiLiteRegBlock` 封装了完整的 AXI-Lite 到寄存器访问的桥接。

---

## 10. Clos/ — 置换网络

路径：`BaseCbb.Clos`

### BenesClos2x2 / BenesClos — Benes 网络

```scala
class BenesClos[T <: Data](dt: T, Num: Int) extends Module
```

递归构建的 Benes 置换网络，支持任意 `Num × Num` 的输入输出排列。

| 信号 | 方向 | 类型 | 说明 |
|------|------|------|------|
| `sel` | Input | Vec[Bool] | 配置位（每 2×2 需要 1 bit） |
| `in` | Input | Vec[T] | 输入数据 |
| `out` | Output | Vec[T] | 输出数据（由 sel 配置决定排列） |

**配置位数量：** `Benes.CfgSize(Num) = Num * log2Ceil(Num)` bits

**递归分解：** NxN → 上半部分 (N/2)x(N/2) + 下半部分 (N/2)x(N/2) + 两个 2×2 crossbar

---

## 11. Area/ — 面积估算

路径：`BaseCbb.Area`

### ProcessConfiguration — 工艺配置

预设了 T7、S12、S7 三种工艺的面积参数：

```scala
ProcessConfiguration.T7.mux2_area      // T7 MUX2 面积
ProcessConfiguration.T7.ff_area       // T7 DFF 面积
ProcessConfiguration.T7.nd2_area       // T7 NAND2 面积
ProcessConfiguration.T7.logic_uti      // 组合逻辑利用率
ProcessConfiguration.T7.mem_uti       // 存储器利用率
```

### GenArea — 面积计算

```scala
case class GenArea(ff_num: Int, comb_area: Double = 0, mem_area: Double = 0)
```

根据 DFF 数量、组合面积和存储面积计算总芯片面积。

---

## 附录：模块依赖关系图

```
BasicCells
    ↓
SequentialUnits ──────────────────────────┐
    ↓                                      │
ArithmeticUnits                            │
    ↓                                      ↓
GeneratorLib ──────► RegCbb/RegType ──► RegFieldDsl ──► RegisterBlockDsl
    ↓                                      │           ↘
utils (LatencyPipe,                        │            RegisterIRGenerator
  ShiftQueue, AsyncResetReg,      AxiInterfaces  AddressAllocator
  ShiftRegEn)                                        ↓
    ↓                                    RegisterFileGenerator
SyncFifos ──► TpMemoryPort ◄─── Memory ──► DescribedSRAM
    │                      (Sp/Tp)           │
    │                                        ↓
Sync2 ◄───────► AsyncPulse ◄──► AsyncBus   Bitmap
    │                                      │
    │                              BitmapCacheMem
    ↓                                      │
AsyncFifos (AsyncFifo,                    │
  AsyncZeroLatencyFifo)                   Linklist
    │
    └──► GrayCounter, AsyncHandshake
    │
    └──► PulseSync, EdgeDetect, AsyncRstSync

Arbiter ──► RR, WRR
  └──► islip ► iSlipLogic, RegulariSlip

RegCbb.dsl ──► GenRegBlock (AxiLiteRegBlock)
```

```mermaid
flowchart LR
A-->B
```