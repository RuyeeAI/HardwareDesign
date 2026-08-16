# basic/ — 基本门级单元与时序单元

> 路径：`src/main/scala/BaseCbb/basic/`　实际包：`BaseCbb.basic`（BasicCells）、`BaseCbb.sequential`（SequentialUnits/ClockDivider，**物理路径与包不一致，见 §7**）、`BaseCbb.utils.cdc`（AsyncResetReg，同上）
> 文件：`BasicCells.scala`（256 行）、`SequentialUnits.scala`（300 行）、`ClockDivider.scala`（57 行）、`AsyncResetReg.scala`（104 行）

---

## 1. BasicCells.scala — 门级单元（`BaseCbb.basic`）

全部为纯组合/简单时序单元，无内部依赖其它 BaseCbb 模块。均带显式 `clk/rst_n` 端口（部分时序单元），
端口命名与标准单元库一致（`a/b/c/d0/d1/sel`、`y/q`）。

### 1.1 基本门（组合）

| 类 | 功能 | 端口 |
|----|------|------|
| `Inv` | 反相器 | in → out |
| `Buf` | 缓冲器 | in → out |
| `And2` / `And3` | 2/3 输入与 | a,b[,c] → y |
| `Nand2` / `Nand3` | 2/3 输入与非 | 同上 |
| `Or2` | 2 输入或 | a,b → y |
| `Nor2` / `Nor3` | 2/3 输入或非 | 同上 |
| `Xor2` / `Xnor2` | 2 输入异或/同或 | a,b → y |
| `Mux2N[T]` | 参数化 2 选 1 | d0,d1,sel → y（`Mux2` 已删除，由 `Mux2N[Bool]` 覆盖） |
| `HalfAdd` / `FullAdd` | 半加/全加器 | a,b[,cin] → sum,cout |
| `AOI22` / `AOI32` | 与或非复合门 | a1,a2[,a3],b1,b2 → y |
| `SRLatch` | SR 锁存器 | s,r → q,qn（组合反馈，**综合会成环**，仅教学/结构示意用） |

### 1.2 时序单元

| 类 | 功能 | 复位 | 端口 |
|----|------|------|------|
| `DLatch` | D 锁存器（电平使能） | - | d,en → q |
| `DFF` | D 触发器（无复位） | - | clk,d → q |
| `DFFAsyncRst` | D 触发器 | 异步低有效 | clk,rst_n,d → q |
| `DFFSyncRst` | D 触发器 | 复位类型为通用 `Reset()`，**实际由连接推断**（注释称"同步复位"但不强制） | clk,rst_n(Reset) → q |
| `ClockGating` | 时钟门控 | - | clk,en → gclk（下降沿锁存使能再与门，避免毛刺） |

---

## 2. SequentialUnits.scala — 时序电路单元（`BaseCbb.sequential`）

### 2.1 Register — N 位寄存器

```scala
class Register(width = 32)   // clk, rst_n(AsyncReset), din, wen, dout
```
`wen=1` 时 `dout := din`，否则保持。**注**：端口里手动暴露 `clk/rst_n` 并用 `withClockAndReset`
包裹——在 Chisel 3.6 中模块已隐式有 clock/reset，此写法会在 IO 上多出两个物理端口；
且 `Reg(UInt(width.W))` **无 RegInit 初值**，`rst_n` 实际不驱动任何复位行为（仅作时钟域声明）。

### 2.2 RegFile — 参数化寄存器堆

```scala
class RegFile(nRead = 1, nWrite = 1, dataWidth = 32, addrWidth = 5)
// wen/waddr/wdata: Vec(nWrite)；ren/raddr/rdata: Vec(nRead)
```
- 内部 `Mem(depth, UInt(dataWidth.W))`，`depth = 1 << addrWidth`；
- 读：`ren(i)` 使能时组合读出 `mem.read(raddr(i))`，否则 0（**异步读，Mem 推断为寄存器堆**）；
- 写：`wen(i)` 时 `mem.write(waddr(i), wdata(i))`（同步写）。
- **组合读 + 同步写**：同一周期读写同地址返回旧值（写入未旁路）。
- ⚠ 读口 `mem.read` 在 `withClock(io.clk)` 作用域**之外**（读用隐式时钟、写用显式 io.clk）——若两时钟不同存在 CDC 风险；多写口同地址冲突行为未定义。

### 2.3 RegFile1R1W / RegFile2R1W — 兼容包装

1 读 1 写 / 2 读 1 写固定端口形态，内部复用 `RegFile`（`RegFile2R1W` 两个读口 `ren` 恒 1——包装层行为不对称，使用者需留意）。
若不需要保持旧接口，可直接用参数化 `RegFile` 替代。

### 2.4 计数器

| 类 | 功能 | 端口 | 说明 |
|----|------|------|------|
| `UpCounter(width=8)` | 二进制递增计数 | clk,rst_n,en,clear → count,carry | 上溢时 `carry = cnt.andR & en`；`clear` 同步清零 |
| `ModNCounter(mod=100)` | 模 N 计数 | clk,rst_n,en → count,overflow | 内部复用 `BaseCbb.math.ZCounter`（**已消除重复实现**），0..N-1 循环，溢出输出 wrap |

### 2.5 时钟分频（行为级，输出 Clock/Bool）

| 类 | 功能 | 输出类型 | 说明 |
|----|------|------|------|
| `ClkDiv2` | 二分频 | Clock | 50% 占空比，`RegInit(false)` 翻转 |
| `ClkDivOdd(div=3)` | 奇数分频 | Clock | 上沿/下沿两计数器组合保证 **50% 占空比**；`require(div%2==1)` |
| `ClkDiv(div=10)` | 任意整数分频 | Bool | 非 50%：计数到 `half-1` 翻转；**奇数分频占空比 ≈ (half+1):half** |

注意：`ClkDiv2/ClkDivOdd/ClkDiv` 输出是**派生时钟**（`asClock`），在真实设计中使用派生时钟
做时钟域划分有 CDC 风险，建议仅用于仿真/低频场景；正式设计应使用下面的 BlackBox 分频或 PLL。

### 2.6 FsmTemplate — 三段式 FSM 模板

```scala
class FsmTemplate   // clk, rst_n, start, doneCond → idle, busy, done, currentState
```
固定三态 `sIDLE/sBUSY/sDONE`（`FsmStates` 枚举）：start→BUSY，doneCond→DONE，DONE→IDLE。
（已修复：原 `stateNum` 参数与写死三态逻辑矛盾，参数已删除。）

---

## 3. ClockDivider.scala — BlackBox 分频（`BaseCbb.sequential`）

| 类 | 功能 |
|----|------|
| `ClockDivider2` | 2 分频 BlackBox（`clk_in → clk_out`，相位对齐；**需工程提供 ClockDivider2.v**） |
| `ClockDivider3` | 3 分频 BlackBox（同上，需 ClockDivider3.v） |
| `Pow2ClockDivider(pow2)` | `2^pow2` 分频：链式级联 `pow2` 个 ClockDivider2；`pow2==0` 直通 |

`Pow2ClockDivider.apply(pow2)` / `apply(clock_in, pow2)` 两个工厂方法。

---

## 4. AsyncResetReg.scala — 异步复位寄存器（`BaseCbb.utils.cdc`）

> 物理在 basic/ 下但包为 `BaseCbb.utils.cdc`（P3 迁移遗留）。

- `AsyncResetReg(resetValue=0)`：**RawModule**，显式 clk/rst/en 端口的 1bit 异步复位寄存器
  （供综合/后端替换用，注释强调"异步复位信号 ≠ 异步复位寄存器，仍需同步释放复位"）。
- `AsyncResetRegVec(w, init)`：w 位异步复位寄存器（`desiredName = AsyncResetRegVec_w{w}_i{init}`，
  `SimpleRegIO(d,q,en)`）。
- `AsyncResetReg.apply` 重载族：单 bit 工厂（`apply(d, clk, rst, init, name)`）与向量工厂
  （`apply(updateData, resetData, enable, name)`，宽度取 `max(w, resetData.bitLength)`）。

**用途**：`misc.AsyncResetShiftReg` 内部逐级实例化 `AsyncResetRegVec`；
`misc.AbstractPipelineReg` 抽象允许后端替换这些寄存器为 metafix/标准单元。

---

## 5. 设计注意与建议

1. **包/目录不一致**（P3 遗留）：`SequentialUnits.scala`/`ClockDivider.scala` 包名为
   `BaseCbb.sequential`，`AsyncResetReg.scala` 包名为 `BaseCbb.utils.cdc`，均与物理目录 `basic/` 不符。
   → 建议统一改为 `BaseCbb.basic`（同步改引用方：SequentialUnitsSpec/ClockDividerSpec 与 README）。
2. `Register` 手动暴露 clk/rst_n 端口与 Chisel 隐式时钟复位并存，是旧风格写法；新代码建议去掉
   显式端口直接用 `withClockAndReset`。
3. `DFF/DFFSyncRst` 与 `Register` 功能重叠（Register 即带使能的多位 DFF）——可考虑 `DFF` 系列保留
   作为单元库门面，`Register` 改由其包装。
4. `ClkDiv` 奇数分频非 50% 占空比，若需 50% 请用 `ClkDivOdd`；综合场景用 BlackBox 分频。
5. 测试：`src/test/scala/BaseCbb/basic/BasicCellsSpec.scala`（164 行）、
   `src/test/scala/BaseCbb/sequential/SequentialUnitsSpec.scala`（40 行）、
   `src/test/scala/BaseCbb/sequential/ClockDividerSpec.scala`（28 行）。
