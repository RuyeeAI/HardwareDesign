# arbiter/ — 仲裁器与调度器

> 路径：`src/main/scala/BaseCbb/arbiter/`　包：`BaseCbb.arbiter`（`arbiter.scala` 例外：声明**根包 `BaseCbb`**，见 §6）
> 文件 3 个：arbiter.scala（RR/WRR）、HellaArbiters.scala（rocket-chip 移植）、islip.scala（iSLIP 调度）

---

## 1. 公共原语：RrLogic（arbiter.scala，根包 `BaseCbb`）

```scala
object RrLogic { def apply(rdy: UInt, point_ff: UInt): UInt }   // 返回 one-hot grant
```
- 双份向量借位公式：`grant = (Cat(rdy,rdy) & ~(Cat(rdy,rdy) - point_ff))` 高低半 OR。
- 语义：从 `point_ff` 位起（**含指针位**）第一个有效请求 → one-hot 授权；跨过指针后可在下一轮次。
- ⚠ **无保护**：`point_ff=0` 或非 one-hot 时静默输出 0（是 RegulariSlip 死锁 bug 的根源，见 §5）。

## 2. RR — 轮询仲裁器（根包 `BaseCbb`）

```scala
class RR(ClientNum: Int)
// ready: UInt(ClientNum)（请求位图）；grant: UInt(ClientNum)（one-hot，组合）；enable: Bool（指针更新使能）
```
- `point_ff = RegInit(1.U)`（合法 one-hot 初值）；`grant = RrLogic(ready, point_ff)`。
- `enable` 时 `point_ff := Cat(grant(N-2,0), grant(N-1))` —— 指针推进为"被选者索引 +1"，**严格轮转、无饥饿**。
- 无握手（每周期组合仲裁），面向单周期调度器内部使用。
- ⚠ `ClientNum=1` 时位切片 `grant(-1,0)` 非法，无 require。

## 3. WRR — 加权轮询（根包 `BaseCbb`）

```scala
class WRR(ClientNum: Int, WtWidth: Int)
// ready/grant/enable 同 RR；weight: Vec(ClientNum, UInt(WtWidth))（每轮配额）
```
- 内部 `wt` 剩余配额寄存器；`req(i) = ready(i) && wt(i)>0`；配额全耗尽时 `load_en` 重载 weight。
- 授权核心**直接例化 RR**：`grant = RR(mask_req, enable)`；被选中者配额减 1。
- **即 WRR = 配额门控 + RR**；`weight=1` 时退化为 RR。

## 4. Hella 族（HellaArbiters.scala，rocket-chip 移植，`BaseCbb.arbiter`）

### 4.1 HellaLockingArbiter — 锁定型仲裁基类

```scala
abstract class HellaLockingArbiter[T <: Data](typ: T, arbN: Int, rr: Boolean = false)
// in: Vec(arbN, Flipped Decoupled)；out: Decoupled
```
- `choice`：`rr=true` 时从 `lockIdx+1` 起找第一个 valid（轮询旋转）；否则固定最低索引优先。
- `in(i).ready = out.ready && chosen===i`（标准 Decoupled，只给被选者 ready）。
- 基类**自身从不置位 locked/lockIdx** —— 锁定策略完全由子类决定。

### 4.2 HellaPeekingArbiter

```scala
(typ, arbN, canUnlock: T => Bool, needsLock: Option[T => Bool] = None, rr = false)
```
- 通过**窥探数据**决定锁定/解锁：`out.fire` 时，未锁且 needsLock(data) → 锁定；canUnlock(data) → 解锁（unlock 优先）。

### 4.3 HellaCountingArbiter

```scala
(typ, arbN, count: Int, needsLock: Option[T => Bool] = None, rr = false)   // require(count > 1)
```
- 锁定后连续服务 `count` 次 fire 自动解锁（`lock_ctr = Counter(count)`）。

## 5. iSLIP 调度（islip.scala，`BaseCbb.arbiter`）

### 5.1 iSlipLogic — 纯组合两阶段核心

```scala
class iSlipLogic(SrcNum, DstNum) extends GenModule
// req: Vec(SrcNum, Vec(DstNum, Bool))；src_ptr/dst_ptr: one-hot 指针
// gnt: 最终匹配（accept 结果）；b_gnt: 目的侧授权（grant 结果）
```
- **阶段 1（grant）**：每目的 d 按 `dst_ptr(d)` 独立 `RrLogic` 选一个源（列转置后）。
- **阶段 2（accept）**：每源 s 按 `src_ptr(s)` 独立 `RrLogic` 接受一个目的。
- 无寄存器（指针由外部提供），算法对应标准 iSLIP 的 request/grant/accept。

### 5.2 RegulariSlip — 完整调度状态机

```scala
class RegulariSlip(SrcNum, DstNum) extends GenModule
// enable: Vec(DstNum, Bool)（目的可用掩码）；req；gnt
```
- 包上 `src_ptr/dst_ptr` 寄存器 + `enable` 掩码 + 指针推进。
- **疑似死锁 bug**：指针初值 `0.U`，而 `RrLogic(rdy, 0) = 0`（零指针授权恒为全 0）→ 首个周期起无授权且永不恢复。对照 `RR` 模块初值 `1.U` 是合法的。**建议初值改为 `1.U`**（需仿真确认）。
- 指针更新存"被选中者本身"而非"+1"，与 `RR` 的推进语义不一致，可能重复选中同一源/目的（饥饿）。

## 6. 组内重叠与质量

| 重叠对 | 结论 |
|--------|------|
| RR vs WRR | WRR 内部复用 RR，已合理 |
| RR vs HellaLockingArbiter(rr=true) | 轮询语义等价；RR 无握手、Hella 带 Decoupled + 锁定 —— 定位不同，保留 |
| iSlipLogic vs RegulariSlip | 两层封装关系，合理 |
| 与 chisel3.util.Arbiter/RRArbiter | BaseCbb.RR 去握手化；Hella 是 rocket-chip 对 RRArbiter 的锁定扩展 —— 均有独立价值 |

**质量问题**：
1. `arbiter.scala` 声明根包 `BaseCbb` 与同目录 `BaseCbb.arbiter` 不一致 → islip 被迫 `import BaseCbb._`。
2. 参数大写开头（`ClientNum/WtWidth/SrcNum/DstNum`）与全库 camelCase 不一致；`RR(val ClientNum)` 有 val 而其它无。
3. 类名 `RegulariSlip` 拼写怪异；`iSlipLogic` 局部 Wire 与 io 同名 `b_gnt`。
4. `RegulariSlip` 指针初值 0 疑似死锁（上文）。
5. `islip.scala:79-80` 注释掉的旧逻辑残留。

**测试**：`ArbiterSpec.scala`（121 行）、`HellaArbitersSpec.scala`（59 行）——注意 ArbiterSpec 的编译/行为测试需在修复指针初值后复核。
