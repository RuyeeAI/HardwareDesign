# fifo/ — FIFO 队列

> 路径：`src/main/scala/BaseCbb/fifo/`　包：`BaseCbb.fifo`　文件 2 个：SyncFifos.scala（272 行）、AsyncFifos.scala（136 行）
> 设计原则：**存储一律外置**——通过 `BaseCbb.memory.TpMemoryPort`/`SpMemoryPort` 连接外部 SRAM，模块本体可仿真可综合。

---

## 1. SyncFifo — 基础同步 FIFO

```scala
class SyncFifo(dataWidth = 32, addrWidth = 4, readLatency: Int = 1)   // require(readLatency ∈ {0,1})
// mem: Flipped TpMemoryPort；clk, rst_n(AsyncReset), wrEn, din, rdEn, dout, empty, full, level
```
- `wrPtr/rdPtr` + `count(addrWidth+1)` 寄存器；`empty = count==0`、`full = count==depth`、`level = count 低位`。
- 读地址：`readLatency=1` 用 rdPtr（dout 打拍）；`=0` 预加 1 且 `dout := mem.rdata` 组合直通（零延迟）。
- ⚠ **行为不一致**：`readLatency=1` 时 `io.dout` 为**条件赋值（组合 mux，非读周期为 0）**，与 AsyncFifo 的 `RegEnable`（寄存器保持）语义不同——两个 "readLatency=1" 行为需统一。
- ⚠ 显式 `io.clk/io.rst_n` 与 Module 隐式时钟/复位并存（应改 RawModule）；且用 `withClockAndReset` 异步复位 + `notRst` 同步清零**双重复位**。

## 2. SyncZeroLatencyFifo — 零延迟兼容子类

```scala
class SyncZeroLatencyFifo(...) extends SyncFifo(dataWidth, addrWidth, readLatency = 0)   // 纯兼容别名
```

## 3. RegisterBasedFifo — 寄存器堆 FIFO

```scala
class RegisterBasedFifo(dataWidth = 32, depth = 8)   // require(depth <= 32)；无 mem 端口
// clk, rst_n, wrEn, din, rdEn, dout, empty, full, level
```
- `regs: Reg(Vec(depth))` 组合读（零延迟）；指针/计数逻辑与 SyncFifo 相同。
- 适用深度 ≤ 32 的小 FIFO。

## 4. DualSPRamFifo / DualSinglePortRamFifo — 双单口 SRAM FIFO

```scala
class DualSPRamFifo(dataWidth = 32, addrWidth = 4)   // require(addrWidth >= 1)
// memBank0/memBank1: Flipped SpMemoryPort(addrWidth-1, dataWidth) + 标准 FIFO 端口
```
- 高位选 bank（`wrBank = wrPtr MSB`）；同 bank 同地址 → 旁路写数据到读输出（1 拍延迟）；
  同 bank 不同地址 → **停读**（rdStall，dout 保持）。
- ⚠ **边界隐患**：冲突元素恰为 bank 末元素（rdPtr 前进使 rdBank 翻转）时，旁路数据被路由到另一 bank 输出（疑似丢数，需仿真验证）。
- `DualSinglePortRamFifo` 为纯兼容子类（行为升级为与 DualSPRamFifo 一致）。

## 5. AsyncFifo — 异步 FIFO（格雷码）

```scala
class AsyncFifo(dataWidth = 32, addrWidth = 4, readLatency: Int = 1)
// mem: Flipped TpMemoryPort；wrClk/wrRst_n/wrEn/din/full/wrLevel；rdClk/rdRst_n/rdEn/dout/empty/rdLevel
```
- 写域：`wrPtrBin/wrPtrGray`；`we = wrEn && !full`；读侧格雷 2 级同步 → `wrLevel = (wrPtrBin - rdBinSync)(低)`；
  `full = 格雷 MSB 与次 MSB 均不同且低位相同`。
- 读域：`re = rdEn && !empty`；`readLatency=1` 时 `dout := RegEnable(mem.rdata, re)`（寄存器保持）；
  `=0` 时预加地址组合直通；`empty = rdPtrGray === wrGraySync2`。
- 类内 `grayToBinary` 逐位异或。

## 6. AsyncZeroLatencyFifo — 兼容子类

```scala
class AsyncZeroLatencyFifo(...) extends AsyncFifo(..., readLatency = 0)   // 纯兼容别名
```

---

## 7. 组内重叠与建议

| 重叠点 | 结论 |
|--------|------|
| SyncFifo / DualSPRamFifo / RegisterBasedFifo 控制逻辑 | **指针/计数/empty/full/level + 入出同拍 switch 逐行重复**（3 处）——**最值得做的去重**：抽共享 `FifoCtrl`（指针+计数生成器），三个 FIFO 只定制存储访问 |
| ZeroLatency 兼容子类 | 已合并完成（固定参数），保留即可 |
| SyncFifo vs AsyncFifo 的 readLatency 机制 | 机制相同但跨时钟域逻辑独立，不宜合并；但 dout 语义需统一 |
| Linklist vs FIFO | 指针追逐队列 vs 环形缓冲（见 memory 文档 §3）——边界清晰，保留 |

## 8. 设计注意

1. **存储外置**是正确设计：TpMemoryPort/SpMemoryPort 统一端口风格。
2. 显式 clk/rst_n + 隐式时钟并存（SyncFifo/AsyncFifo 同病）——建议改 RawModule 或去掉显式端口。
3. `halfDepth`（DualSPRamFifo）未使用（死变量）。
4. **测试**：SyncFifosSpec（30 行）/ AsyncFifosSpec（28 行）——覆盖较薄，SyncFifo 读延迟 1/0、DualSP 边界、AsyncFifo 满空判断建议补测。
