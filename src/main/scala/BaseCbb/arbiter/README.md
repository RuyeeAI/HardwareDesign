# Arbiter 模块

## 概览

路径: `BaseCbb.arbiter`

提供 RR (Round-Robin)、WRR (Weighted Round-Robin) 和 iSlip 迭代调度仲裁器。

---

## RR — 轮询仲裁器

```scala
class RR(ClientNum: Int) extends Module
```

固定优先级轮询仲裁器。每次 grant 后指针旋转到下一位，下次优先响应下一个客户。

| 信号 | 方向 | 类型 | 说明 |
|------|------|------|------|
| `ready` | Input | UInt(ClientNum.W) | 各客户请求 |
| `grant` | Output | UInt(ClientNum.W) | 独热 grant |
| `enable` | Input | Bool | 使能（=0 时保持 grant 不变） |

**算法**: 使用 double-grant 技巧 — 将 ready 加倍拼接后，通过 `& (~(double_rdy - point_ff))` 提取最低优先级的 ready 位，再折叠回原始宽度。

**companion object** 提供 `RR.apply(rdy, en, name)` 工厂方法。

---

## WRR — 加权轮询仲裁器

```scala
class WRR(ClientNum: Int, WtWidth: Int) extends Module
```

支持权重的轮询仲裁。内部使用 RR 进行每轮调度，每个客户消耗完权重后被屏蔽，所有权重耗尽后重新装载。

| 额外信号 | 方向 | 类型 | 说明 |
|----------|------|------|------|
| `weight` | Input | Vec(ClientNum, UInt(WtWidth.W)) | 各客户权重 |

---

## iSlip — 迭代调度器

```scala
class iSlipLogic(SrcNum: Int, DstNum: Int) extends GenModule
class RegulariSlip(SrcNum: Int, DstNum: Int) extends GenModule
```

用于 crossbar 交换芯片的并行迭代匹配算法:

1. **Stage 1（目标侧）**: 每个输出从 `dst_ptr` 开始轮询，选择第一个有请求的输入
2. **Stage 2（输入侧）**: 每个输入从 `src_ptr` 开始轮询，选择第一个有请求的输出

`RegulariSlip` 在 `iSlipLogic` 基础上增加了指针寄存器和 enable 掩码控制。

---

## HellaLockingArbiter — 带锁轮询仲裁器

```scala
abstract class HellaLockingArbiter[T <: Data](typ: T, arbN: Int, rr: Boolean = false)
```

泛型锁定仲裁器基类。可选 RR 模式，锁定时保持选中的客户端。

### HellaPeekingArbiter — 窥探式解锁仲裁器

```scala
class HellaPeekingArbiter[T <: Data](typ: T, arbN: Int, canUnlock: T => Bool, needsLock: Option[T => Bool] = None, rr: Boolean = false)
```

通过检查数据内容判断是否可解锁。当 `canUnlock(data)` 为 true 时释放锁定。

### HellaCountingArbiter — 计数式解锁仲裁器

```scala
class HellaCountingArbiter[T <: Data](typ: T, arbN: Int, count: Int, needsLock: Option[T => Bool] = None, rr: Boolean = false)
```

锁定固定 `count` 个事务后释放。适用于多拍事务（如 burst 传输）。
