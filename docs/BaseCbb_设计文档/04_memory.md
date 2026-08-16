# memory/ — 存储模型、SRAM 封装与分配器

> 路径：`src/main/scala/BaseCbb/memory/`　包：`BaseCbb.memory`　文件 6 个：
> Memory.scala（947 行，含配置/端口/BB/仿真/ECC/Wrap3）、BitmapCacheMem、BitmapKernel、IDPool、Linklist、Bitmap（已恢复）

---

## 1. Memory.scala — 存储配置与封装体系

### 1.1 配置枚举

| 对象 | 值 |
|------|-----|
| `MemoryAccessType` | SP（单口）/ TP（双口）/ DP / TCAM |
| `MemoryProtectType` | ECC / Parity / ProtNone |
| `MemoryInitType` | AllZero / AllOne / Incr（⚠ `initValue` 参数从未被消费） |

⚠ `MemoryAccessType` 与 `RegCbb.Def.MemoryAccessType`（sealed trait）**同名撞名**。

### 1.2 case class Memory — 配置对象

```scala
case class Memory(name, dataType: Data, depth, memoryType = SP, instNum = 1, Hazard = false, Fatal = false,
                  RsAccess = false, initValue = AllZero, flopIn = false, flopOut = true, CheckIn = false,
                  CheckOut = true, protect = ECC, isPhysicalMemory = false, protectWidthTh = 320,
                  bypassOnConflict = false, RsMemoryDisLat = 32)
```
- 计算属性：`dataWidth`（原始宽 + ECC/Parity 开销）、`latency = 1 + flopIn + flopOut`（不含 CheckIn/Out）、
  `addrWidth = log2Ceil(depth)`、`eccWidth(n)`（SECDED：最小 k 使 2^k ≥ n+k+1）、`eccSegNum/eccSegWidth`。
- ⚠ 死配置：`Hazard/Fatal/initValue/memoryType/instNum/toMap/lastCheckSegWidth` 无调用方；
  私有 `log2Ceil` 与 `EccCodec.eccWidthOf` 公式**重复实现**（两处，易漂移）。

### 1.3 端口类

| 类 | 端口 | 说明 |
|----|------|------|
| `SpMemoryPort` | we/re/addr/wdata/rdata | 单口逻辑端口（extends GenBundle） |
| `TpMemoryPort` | we/re/waddr/raddr/wdata/rdata | 双口 |
| `SpMemoryLgcPort` | Sp + uecErr | 带 ECC 不可纠错误上报（Wrap3 用） |
| `TpMemoryLgcPort` | Tp + uecErr | 同上 |
| `MemoryDfxPort` | init/initDone/eccErr/eccUerr/eccErrAddr/injCorrEn/injUerrEn/injDone | 初始化+错误注入 |
| `CpuRsPort` | re/we/addr/wdata/rdata/ack/status(2b) | CPU 调试访问（⚠ 用 Bundle 而非 GenBundle，风格不一致） |

### 1.4 实现类

| 类 | 说明 |
|----|------|
| `SpMemoryBB`/`TpMemoryBB` | 物理 SRAM BlackBox 占位（无 Verilog 模板） |
| `SimMemory` | 行为模型：`Reg(Vec)`，`rdata := RegNext(mem(raddr))` 固定 1 拍读延迟 |
| `MemoryWrap` | ⚠ **空壳基类**（README 声称的 `MEM_TYPE` 成员不存在）——建议删除或改 trait |
| `SpMemoryWrap` | 单口封装：输入流水（RegNext/RegEnable）+ isPhysicalMemory 二选一 + 输出打拍；总读延迟 = mem.latency |
| `TpMemoryWrap` | 双口封装；⚠ 输出 RegNext 链在 `withClockAndReset` 外（RawModule 无隐式时钟），物理分支疑似无法编译 |
| `SpMemoryWrap3`/`TpMemoryWrap3` | ECC/Parity + DFX 初始化/错误注入 + CPU 访问仲裁（见 1.5） |

### 1.5 Wrap3 — 保护封装 + CPU 仲裁

层次：`User → Wrap3 → Wrap → BB/SimMemory`。

- **CheckIn 打拍**：`wdata/addr := RegEnable(_, we)`、`we/re := RegNext`——⚠ SP 版 `addr` 仅按 `we` 采样，
  **纯读访问的地址不被捕获**（疑似 bug；TP 版 `raddrFlopped = RegEnable(_, re)` 正确）。
- **初始化 FSM**（sIdle/sInit）：`init` 触发逐地址写 0；与用户读写共享输入 mux。
- **ECC 解码**：`EccCodec.decodeAndCheck` 连续解码，CPU 读回 `decData`。
- **CPU FSM**（sCpuIdle/sCpuWait/sCpuAccess/sCpuDone）：用户忙时等待；超 `RsMemoryDisLat` → `ack`+rdata 全 1+status=3；
  否则 `cpuAccessCnt==mem.latency` 完成，`status = (re && uerr)?1:0`；`ack` 单拍脉冲。
- **错误注入**：`injCorrReq = injCorrEn && reFlopped` 单拍脉冲按 latency 移位 OR 进 err 输出；
  ⚠ `injDone := injCorrReq || injUerrReq` 是请求回显，非"注入完成"信号（命名误导）。
- **TP 版旁路**：`sameAddrRW = bypassOnConflict && weFlopped && reFlopped && waddr==raddr` → 旁路写数据到读输出。

### 1.6 EccCodec — ECC/Parity 编解码

| 函数 | 说明 |
|------|------|
| `eccWidthOf(segBits)` | 与 `Memory.eccWidth` 公式重复 |
| `encodeParity/decodeParity` | 每段 xorR 奇偶；⚠ decodeParity 返回 `(data, err, err)` —— **err 与 uerr 相同**（奇偶无法区分） |
| `encodeEcc/encodeEccSeg` | Hamming SECDED：k 校验位 + 1 整体偶校验；`require(k >= 3)` |
| `decodeEccSeg` | syndrome 纠正 1bit（`data ^ (1 << (syndrome-1))`）；`uerr = syndromeNonZero && !parityMismatch` |
| `decodeAndCheck` | 按 protect 分发（ProtNone/Parity/ECC） |

---

## 2. 位图分配器家族

### 2.1 BitmapKernel — 共享组合内核（1=可用）

```scala
object BitmapKernel {
  firstFree(bitmap)   = PriorityEncoder            // 最低可用位
  hasFree(bitmap)     = bitmap.orR
  isEmpty(bitmap)     = bitmap.andR                // 全可用
  isFull(bitmap)      = !bitmap.orR                // 全占
  allocUpdate(bitmap, idx) = bitmap & ~UIntToOH(idx)  // 占用清 0
  freeUpdate(bitmap, idx)  = bitmap | UIntToOH(idx)   // 释放置 1
  freeCount(bitmap)   = PopCount
}
```
- 语义统一为 **1=可用**；Bitmap/IDPool/BitmapCacheMem 共用，消除三处重复实现。

### 2.2 Bitmap — 寄存器内建分配器（本会话恢复）

```scala
class Bitmap(RscNum) extends GenModule   // req_vld→req_ptr；ret_vld/ret_ptr；empty/full
```
- `bitmap = RegInit(全 1)`；`req_ptr = firstFree`；分配清 0（`& ~UIntToOH`）、释放置 1（`| UIntToOH`），
  同拍先或后与（`(bitmap|set) & ~clr`）；`empty/full` 用内核函数。
- 背景：该文件在用户重组提交中被误删（BitmapSpec/README 仍引用），本会话已恢复。

### 2.3 IDPool — 握手式 ID 分配器（rocket-chip 移植）

```scala
class IDPool(numIds, lateValid = false, revocableSelect = false)
// free: Flipped Valid(id)；alloc: Decoupled(id)
```
- `bitmap = RegInit(全 1)`；`alloc.valid = lateValid ? hasFree : valid`；`bits = revocableSelect ? firstFree : select`。
- 时钟门控更新（仅 `alloc.ready || free.valid` 时）；防护断言：禁止双重释放、valid 一致性、select 一致性。
- 定位：**小容量寄存器池**（与 BitmapCacheMem 的大容量 SRAM 分配互补）。

### 2.4 BitmapCacheMem — 带行缓存的 SRAM 位图分配器

```scala
class BitmapCacheMem(n, cacheSize = 64, memLatency = 1)   // n % cacheSize == 0
// mem: Flipped TpMemoryPort(log2M, cacheSize)；alloc_req/alloc_ptr/alloc_valid；free_req/free_ptr；init；empty/full/freeCnt
```
- 内部：缓存一行 `cacheData(Vec(cacheSize))` + `cacheTag` + `cacheValid`；`M = n/cacheSize` 行。
- **FSM 四态**：sIdle（命中→组合分配/释放；miss→sRead）、sRead（流水线读行，`latCnt % memLatency == 0` 处理）、
  sWrite（脏行写回）、sInit（逐行写全 1 初始化）。
- ⚠ **已知问题**：
  1. **sWrite 为死状态**（无任何转移进入）；
  2. `latCnt` 触发疑似 **off-by-one**：进入 sRead 首拍 `latCnt=1`，memLatency=1 时立即触发，且处理的 rdata 属于更早的 req_row（错位风险，需仿真确认）；
  3. **sIdle 分配命中不回写 SRAM** → 行被逐出后位图复活，潜在双重分配；
  4. `full/empty` 仅缓存行级近似；`freeCnt` 未缓存时返回 cacheSize（应约等于 n）；
  5. 遗留 `printf` 调试语句会进入生成网表。

---

## 3. Linklist.scala — 链表队列

| 类 | 说明 |
|----|------|
| `SubLinklist(RamLat, RscNum, PtrW)` | 单级链表：SRAM 存"下一跳"；head/tail 寄存器；`link_mem_rdata_vld = ShiftRegister(re, RamLat)` 延迟对齐；末元素不出 RAM |
| `LinkList(RamLat, RscNum, PtrW)` | `SubLlNum = RamLat` 个子链表轮转入出，容忍 RamLat 拍读延迟；`ll_cnt` 计数满/空 |
| `VoqLinkList(QueueNum, RamLat, RscNum, PtrW)` | VOQ 场景：每队列独立入队、多队列并行出队；`SubLlNum = RamLat*QueueNum` |

- **与 FIFO 边界**：LinkList 是指针追逐队列（任意 RamLat、暴露 head/tail、无 level）；SyncFifo 是环形缓冲
  （固定 0/1 读延迟、有 level）——分工明确，保留两者。
- ⚠ **VoqLinkList 疑似索引 bug**：使能侧 `i%RamLat`/`floor(i/QueueNum)` 与选择器侧 `i%QueueNum`/`ceil(i/RamLat)`
  不一致，仅 `RamLat == QueueNum` 时三者一致（否则子链表映射错乱）。
- ⚠ `import firrtl.PrimOps.Pad` 未使用（直接依赖 firrtl 内部包）。

---

## 4. 与文档/测试的脱节（需修复）

1. `src/test/scala/BaseCbb/memory/BitmapSpec.scala` 引用 `new Bitmap(8)`——本会话已恢复 Bitmap.scala 并通过。
2. `memory/README.md` 记载的 `MEM_TYPE`（MemoryWrap）不存在；`Bitmap` 的 empty/full 语义描述（1=占用）与
   BitmapKernel（1=可用）**颠倒**，重构后文档未同步。
3. `async/README.md` 与 `BaseCbb/README.md` 记载的 `AsyncFifoCore`（握手式异步 FIFO）**无对应代码**。
4. **测试清单**：BitmapSpec / MemorySpec / MemoryCpuSpec / SpMemoryEccWrapSpec / TpMemoryWrap3Spec / DebugInjSpec /
   DebugInj2Spec / IDPoolSpec（在 utils/ 下）——其中 MemoryCpuSpec 等大测试（400+ 行）覆盖 CPU 仲裁与 ECC。
