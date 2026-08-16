# BaseCbb Memory 模块

路径: `BaseCbb.memory`

提供通用 SRAM 封装（仿真/物理模式）、ECC 保护、Bitmap 资源分配器和链表队列。

---

## 1. Memory.scala — 通用存储器封装

### Memory — 配置对象 (case class)

| 参数 | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| `name` | String | — | 实例名称 |
| `dataType` | Data | — | 数据类型 |
| `depth` | Int | — | 深度（字数） |
| `memoryType` | MemoryAccessType | `SP` | 端口类型: `SP` / `TP` / `DP` / `TCAM` |
| `flopIn` | Boolean | `false` | 输入打拍 |
| `flopOut` | Boolean | `true` | 输出打拍 |
| `protect` | MemoryProtectType | `ECC` | 保护方式: `ECC` / `Parity` / `ProtNone` |
| `CheckIn` | Boolean | `false` | ECC 层输入检查 |
| `CheckOut` | Boolean | `true` | ECC 层输出检查 |
| `protectWidthTh` | Int | `320` | ECC 分段阈值 |
| `isPhysicalMemory` | Boolean | `false` | true=物理 BB / false=SimMemory |
| `bypassOnConflict` | Boolean | `false` | 同地址读写旁路（TP） |
| `RsAccess` | Boolean | `false` | 是否启用 CPU(Rs) 访问仲裁 |

**计算属性**: `dataWidth` (含 ECC/Parity 开销), `latency` (1 + flopIn + flopOut), `addrWidth` (log2Ceil(depth))

### 接口类

| 类 | 说明 |
|----|------|
| `SpMemoryPort(addrWidth, dataWidth)` | 单口存储接口 (we/re/addr/wdata/rdata) |
| `TpMemoryPort(addrWidth, dataWidth)` | 双口存储接口 (we/re/waddr/raddr/wdata/rdata) |
| `SpMemoryLgcPort` / `TpMemoryLgcPort` | 带 ECC 不可纠正错误上报的接口 |
| `MemoryDfxPort(addrWidth)` | 初始化控制与 ECC 错误状态接口 |
| `SpMemoryBB(mem)` | 单口 SRAM BlackBox |
| `TpMemoryBB(mem)` | 双口 SRAM BlackBox |
| `SimMemory(dataWidth, depth)` | 仿真模型 (基于 Vec 寄存器堆) |

### 封装层

**SpMemoryWrap(mem)** / **TpMemoryWrap(mem)**: 根据 `mem.isPhysicalMemory` 切换仿真(`SimMemory`)/物理(`*MemoryBB`)实现，支持输入/输出插拍（`flopIn`/`flopOut`）。总读延迟 = 1 + flopIn + flopOut。

**SpMemoryWrap3(mem)** / **TpMemoryWrap3(mem)**: ECC/Parity 保护封装。层次: `User Logic → Wrap3 → Wrap → BB/SimMemory`。支持初始化和错误注入。

### EccCodec

ECC/Parity 编解码公共函数:
- `encodeEcc` / `decodeEccMultiSeg` — 多段 ECC 编解码
- `encodeParity` / `decodeParity` — 奇偶校验
- `decodeAndCheck` — 统一校验入口

---

## 2. Bitmap.scala — Bitmap 资源分配器

位图语义：**1 = 可用，0 = 已分配**；初始全 1（全可用）。组合内核复用 `BitmapKernel`（与 IDPool / BitmapCacheMem 统一）。

```scala
class Bitmap(RscNum: Int) extends GenModule
```

| 信号 | 方向 | 说明 |
|------|------|------|
| `req_vld` | Input | 分配请求 |
| `req_ptr` | Output | 分配的资源指针 (PriorityEncoder 选择最低空闲位) |
| `ret_vld` | Input | 归还请求 |
| `ret_ptr` | Input | 归还的指针 |
| `empty` | Output | 无可用资源 (bitmap 全 0，全占) |
| `full` | Output | 全部空闲 (bitmap 全 1，全可用) |

---

## 3. BitmapCacheMem.scala — 带缓存的 Bitmap 分配器

```scala
class BitmapCacheMem(n: Int, cacheSize: Int = 64, memLatency: Int = 1) extends Module
```

将 n 个 bit 存储在外部 SRAM 中，模块内部缓存一行 (cacheSize bits)，减少 SRAM 访存次数。n 必须被 cacheSize 整除。

| 信号 | 方向 | 说明 |
|------|------|------|
| `mem` | TpMemoryPort | 外部 SRAM 接口 |
| `alloc_req` / `alloc_ptr` / `alloc_valid` | — | 分配请求/指针/有效 |
| `free_req` / `free_ptr` | — | 释放请求/指针 |
| `init` | Input | 初始化 (清零 SRAM) |
| `empty` / `full` / `freeCnt` | Output | 状态 |

**状态机**: `sIdle → sRead → sWrite → sInit`

---

## 4. Linklist.scala — 链表队列

### SubLinklist — 单级子链表

```scala
class SubLinklist(RamLat: Int, RscNum: Int, PtrW: Int) extends GenModule
```

用 ShiftRegister 处理 RAM 读延迟。

### LinkList — 多级并行子链表

```scala
class LinkList(RamLat: Int, RscNum: Int, PtrW: Int) extends GenModule
```

`SubLlNum = RamLat` 个子链表并行工作，RAM 接口通过 `reduceTree` 合并。

### VoqLinkList — VOQ 链表

```scala
class VoqLinkList(QueueNum: Int, RamLat: Int, RscNum: Int, PtrW: Int) extends GenModule
```

Virtual Output Queue 场景，支持 `QueueNum` 个队列并发出队。`SubLlNum = RamLat × QueueNum` 个子链表。
