# math/ — 算术与数学工具

> 路径：`src/main/scala/BaseCbb/math/`　包：`BaseCbb.math`　文件 9 个：
> ArithmeticUnits / Checksum / Compress / Counters / Crc / Lfsr / MuxLiteral / PrefixSum / ReduceOthers
> 依赖关系：math → basic（FullAdd）单向；math 内部 Compress→PrefixSum、Icrc→Crc、SequentialUnits→ZCounter。

---

## 1. ArithmeticUnits.scala — 算术单元

| 类 | 功能 | 说明 |
|----|------|------|
| `RippleCarryAdder(width=32)` | 逐位进位加法器 | 每位例化 `basic.FullAdd`；`carry(0)=cin`；O(N) 延迟 |
| `CarrySelectAdder(width=32, blockSize=4)` | 进位选择加法器 | 每块两个 RCA（cin=0/1）按块进位 Mux 选择；块间仍进位链 |
| `Subtractor(width=32)` | 减法器 | `a + ~b + 1`（RCA cin=1）；`borrowOut = cout` |
| `AddSub(width=32)` | 加减一体 | `bXor = b ^ Fill(sub)`；cin=sub |
| `Comparator(width=32)` | 无符号比较 | eq/gt/lt 直接比较算子 |
| `Multiplier(widthA,widthB)` | 行为级乘法 | `a * b` 交给综合；`Multipler` 为 @deprecated 别名 |
| `LeftShifter(width)` | 左移 | `din << shamt`（log2Ceil(width) 位 shamt） |
| `RightShifter(width, arithmetic=false)` | 右移 | 算术右移符号扩展 |

## 2. Checksum.scala — 16 位互联网校验和（RFC 1071）

```scala
class Checksum(dataWidth = 16)   // data/valid/first/last → sum/result/ready(恒真)
```
- 17 位累加器端回进位：`raw = acc + data`，`nextSum = Mux(raw(16), raw(15,0)+1, raw(15,0))`；
  `first` 时直接取 data；`last && valid` 时 `result := ~nextSum`。
- ⚠ `ready` 恒 true（无背压语义，冗余端口）。

## 3. Compress.scala — 数组压缩/分散（基于前缀和）

```scala
class Compress[T](gen, n)   // in/valid → out/count（valid 元素压缩到 LSB 侧）
class Scatter[T](gen, n)    // in/mask → out（逆操作，按 mask 散布）
```
- 路由：`DensePrefixSum(validInts)(_+_)` 求前缀和，每个输出口 O(N) MuxCase 选源 —— **O(N²) 组合路由**，N 大时面积显著。
- `count := psum.last`；未命中输出 0。

## 4. Counters.scala — 计数器

| 符号 | 说明 |
|------|------|
| `class ZCounter(n)` | **非 Module 内联片段**：`value` RegInit + `inc(): Bool` 返回回绕；2 的幂靠自然溢出，非 2 幂显式清零；`n==1` 返回恒 true 且 0 位宽 |
| `object ZCounter` | `apply(n)` / `apply(cond, n): (UInt, Bool)`（`when(cond){ wrap = c.inc() }`）——被 `SequentialUnits.ModNCounter` 引用 |
| `object TwoWayCounter` | `apply(up, down, max): UInt`：上下计数；无饱和/回绕处理；上下同拍保持 |
| `case class WideCounter(width, inc=1, reset=true, inhibit=false)` | **低功耗宽计数**：LSB 进位门控 MSB（`nextSmall(smallWidth)` 才递增 large）；`carryOut` lazy val；`:=` 整体赋值 |

## 5. Crc.scala — CRC 生成与校验

```scala
class Crc(polyWidth = 32, poly = 0x04C11DB7L)   // data(8b)/valid/first/init → crc
class Icrc(polyWidth = 32, poly = 0x04C11DB7L)  // + crcIn/crcVld → icrc
```
- `Crc`：LFSR 架构，每周期 8 步（MSB 先行），"移出位为 1 异或多项式"；`first` 时载入 init。
- `Icrc`：接收端用法——`crcVld` 时喂入 `crcIn(7,0)` 最低字节，**调用方需按字节分多拍追加 CRC**（文档未说明该约定）。

## 6. Lfsr.scala — Galois LFSR

```scala
class Lfsr(width = 16)   // seed/load/en → out
```
- `polyTap`：8→0x8B、16→0x2D、24→0x20D、32→0x400007；未匹配默认 0x13。
- `state = RegInit(1)`；`en` 时右移，LSB=1 异或 tap。
- ⚠ **注释与常量不一致**（见 §8 质量 3）：8/16/24 位 polyTap 的注释 tap 位与实际取值对不上，"本原多项式"断言与 32 位"CRC-32 同款"注释均需核对。

## 7. MuxLiteral.scala — 字面量键查表

| 对象 | 键来源 | 说明 |
|------|--------|------|
| `MuxLiteral(index, default, (k,v)*)` | UInt **字面量**键（litValue） | 非字面量在 elaboration 期抛错 |
| `MuxSeq(index, default, v*)` | 位置隐式编号（0,1,2…） | |
| `MuxTable(index, default, (BigInt,v)*)` | BigInt 键 | 核心：键≥0 且互异；**稠密**（endIndex ≤ 2n）→ `VecInit` 索引；**稀疏** → switch 级联；与 default 等值项被过滤 |

- 与 `misc.MuxTLookup` 的关系：MuxTLookup 键可运行时比较（线性深度、支持元组值）；MuxTable 要求静态字面量键（面积/延迟优化）。**静态键场景建议用 MuxTable**（详见重复分析 §3）。

## 8. PrefixSum.scala — 前缀和网络

| 对象 | 层数 | 面积/深度 | 说明 |
|------|------|----------|------|
| `trait PrefixSum` | - | - | 骨架：`apply[T](summands)(op, layerOp)`，`layerOp` 每层回调（可插寄存器） |
| `RipplePrefixSum` | size | N/ N | 逐级 |
| `DensePrefixSum` | 1+log2Ceil(size) | NlogN / logN | **Compress/Scatter 消费** |
| `SparsePrefixSum` | 2logN±1 | 2N / 2logN | contract + expand 两阶段 |

- `TestPrefixSums`（main 内自检）：Scala 级验证 ripple==dense==sparse（0..518 规模）——**建议移到 src/test**。

## 9. ReduceOthers.scala — 归约其余项

```scala
object ReduceOthers { def apply(x: Seq[Bool]): Seq[Bool] }   // out[i] = AND_{j≠i} in[j]
```
- 字面量优化：≥2 个假字面量 → 全 false；1 个 → 仅对应位置输出其余与；
- 变量部分：两两与分组递归 `helper`，`out(i) = x(i^1) && half(i/2)`（异或配对复用子结果，O(NlogN)）。

---

## 10. 组内重叠结论

| 重叠对 | 结论 |
|--------|------|
| UpCounter vs ZCounter vs ModNCounter | `ModNCounter` 已包装 ZCounter；`UpCounter` 手写 `cnt+1` 与 ZCounter 重复 —— **建议 UpCounter 改包装 ZCounter 或与 ModNCounter 合并** |
| HalfAdd/FullAdd vs RCA/CSA/Sub/AddSub | 单元→结构层次，非重复（math→basic 单向依赖） |
| MuxLiteral/MuxSeq/MuxTable | 同一机制三入口，非重复（便捷封装） |
| Checksum/Crc/Lfsr | 算法不同，均保留 |
| Compress/Scatter vs PrefixSum | 消费关系，非重复 |
| ClkDiv2/ClkDivOdd/ClkDiv vs ClockDivider2/3/Pow2 | **功能重复**（行为级 vs BlackBox 级）：ClkDiv2≈ClockDivider2、ClkDivOdd(3)≈ClockDivider3；建议保留行为版+BlackBox 版各一，`ClkDiv` 输出改 Clock 并统一命名 |

## 11. 质量与一致性注意

1. **包/目录错位**：`basic/ClockDivider.scala`、`basic/SequentialUnits.scala` 声明 `BaseCbb.sequential`；`basic/AsyncResetReg.scala` 声明 `BaseCbb.utils.cdc`。
2. `SequentialUnits.scala:137` 注释 "utils.ZCounter" 陈旧（实际 `BaseCbb.math.ZCounter`）。
3. **Lfsr polyTap 注释与常量不符**（8/16/24/32 位均有出入）；测试 LfsrSpec 仅验证可复位/随机性，未必验证最大长度。
4. `TestPrefixSums` 混入 main；`Multipler` @deprecated 别名仍在测试中使用。
5. `ZCounter.apply(cond, n)` 的 `var wrap: Bool = null` 写法易误读。
6. **测试覆盖**：ArithmeticUnitsSpec（219 行）/ ChecksumSpec / LfsrSpec / CompressSpec（含 Scatter 往返）/ CrcSpec 有测试；**MuxLiteral、PrefixSum、ReduceOthers 无专门测试**。
