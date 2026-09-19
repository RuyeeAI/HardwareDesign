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
- 计算属性：`dataWidth`（原始宽 + ECC/Parity 开销）、`latency`、`readLatency`、
  `addrWidth = log2Ceil(depth)`、`eccWidth(n)`（SECDED：最小 k 使 2^k ≥ n+k+1）、`eccSegNum/eccSegWidth`。
- **⚠ 两个"延时"别混用**（实测确认，见 §1.5）：
  - `latency = 1 + flopIn + flopOut` —— **存储链路**延时，**不含 CheckIn/CheckOut**。
    它对齐的节点是"CheckIn 之后的输入寄存器 → ECC 译码之前的裸数据"，
    `MemInitCpuAccess` 的 `cpuAccessCnt == latency` 与 Wrap3 里的 `gateReg`/`bypassValid` 都按这个节点，
    **不要改它的含义**。
  - `readLatency = latency + (CheckIn?1) + (CheckOut?1)` —— **端到端读延时**：
    用户侧 `re` 拉高那一拍到 `rdata`（已纠错）可用的拍数 = `CheckIn + flopIn + 1(存储固有) + flopOut + CheckOut`。
    全关 = 1 拍；**默认配置（flopOut/CheckOut 开）= 3 拍**。
    **消费方要按这个数插流水级**，用 `latency` 会少算 CheckIn/CheckOut。
  - ECC/Parity 只加位宽、编解码是组合的，**不增加读延时**（实测）。
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
| `SpMemoryWrap` | 单口封装：输入流水（RegNext/RegEnable）+ isPhysicalMemory 二选一 + 输出打拍；自身读延时 = `mem.latency` |
| `TpMemoryWrap` | 双口封装；⚠ 输出 RegNext 链在 `withClockAndReset` 外（RawModule 无隐式时钟），物理分支疑似无法编译 |
| `SpMemoryWrap3`/`TpMemoryWrap3` | ECC/Parity + DFX 初始化/错误注入 + CPU 访问仲裁（见 1.5） |

### 1.5 Wrap3 — 保护封装 + CPU 仲裁

层次：`User → Wrap3 → Wrap → BB/SimMemory`。
读延时：`readLatency = CheckIn + flopIn + 1 + flopOut + CheckOut`（见 §1.2），
`Sp`/`Tp` 两版行为一致（已逐配置实测，见 `MemoryWrap3ReadPathSpec`）。

- **CheckIn 打拍**：`wdata/addr := RegEnable(_, we)`、`we/re := RegNext`。
  - ⚠ **SP 版的 `addr` 必须按 `we || re` 采样**（SP 读写共用一个地址）。曾经的写法只按 `we`，
    纯读访问的地址根本不被捕获 → **读任意地址都返回"最后一次写入的地址"的数据**。
    已修（TP 版 `raddrFlopped` 按 `re`、`waddrFlopped` 按 `we`，本来就是对的）。
    回归在 `MemoryWrap3ReadPathSpec`（写 A 再写 B，然后读 A，必须读回 A）。
- **初始化 FSM**（sIdle/sInit）：`init` 触发逐地址写 0；与用户读写共享输入 mux。
- **ECC 解码**：`EccCodec.decodeAndCheck` 连续解码（组合），CPU 读回 `decData`。
- **CPU FSM**（sCpuIdle/sCpuWait/sCpuAccess/sCpuDone）：用户忙时等待；超 `RsMemoryDisLat` → `ack`+rdata 全 1+status=3；
  否则 `cpuAccessCnt==mem.latency` 完成（注意这里对齐的是 **decData** 节点，所以用 `latency` 是对的），
  `status = (re && uerr)?1:0`；`ack` 单拍脉冲。
- **错误注入**：`injCorrReq = injCorrEn && reFlopped` 单拍脉冲按 latency 移位 OR 进 err 输出；
  ⚠ `injDone := injCorrReq || injUerrReq` 是请求回显，非"注入完成"信号（命名误导）。
  ⚠ 注入**不受 protect 档位限制**（`uerrOut = uerr || injUerrPipe`），所以 ProtNone 也能注出 UE。
- **TP 版旁路**：`sameAddrRW = bypassOnConflict && weFlopped && reFlopped && waddr==raddr` → 旁路写数据到读输出；
  旁路命中时错误输出被屏蔽（写数据本就没错）。

#### UE（不可纠错误）的语义与消费

| 端口 | 语义 |
|------|------|
| `lgc.uecErr` | 单拍脉冲：**这一拍输出的 `rdata` 不可纠**。与 `rdata` **同拍**（`uerrOutReg`/`rdataOutReg` 对齐） |
| `dfx.eccErr` | 单比特错（已纠正，`rdata` 是对的） |
| `dfx.eccUerr` | 同 `uecErr`，但被 `cpuBlockUser` 屏蔽（CPU 访问期间不外报） |
| `dfx.eccErrAddr` | **出错那次读的地址**，非报错的拍保持上一次的值（便于事后读 DFX）。无错时为 0 |

- ⚠ `eccErrAddr` 曾经报的是"报错当拍正在发起的**下一个**读地址"（实测：读 3 报 0、读 9 报 3）。
  根因是直接抓 `addrFlopped`，而读地址在发起读之后就不再变。已修为
  `ShiftRegister(实际送进 SRAM 的读地址, mem.latency)` 再随 CheckOut 延一拍，
  报错那拍**直出**、其余拍保持。回归在 `MemoryWrap3ReadPathSpec`。
- ⚠ **UE 时 `rdata` 仍是"看起来合法"的错值**（`decodeEccSeg` 对 UE 不做修改）。
  消费方**必须同拍采 `uecErr`** 并据此丢弃数据；不能指望从数据本身分辨。
  若下游容易漏采，考虑给 `lgc` 加一个随数据走的 poison 位。
- CPU 通路只给了 1 bit `status`（`re && uerr`），**不含地址**；SW 要知道地址得另接 `dfx.eccErrAddr`。

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
   另有 **`MemoryWrap3ReadPathSpec`** 专测读通路契约：读延时 == `readLatency`（TP/SP × 7 组配置，且读回正确数据）、
   ECC/Parity 不改延时、SP 的 CheckIn 能读到请求的地址、`eccErrAddr` 报的是出错那次读的地址。
   （这几条都是踩过坑才补的，见 §1.2/§1.5。）
