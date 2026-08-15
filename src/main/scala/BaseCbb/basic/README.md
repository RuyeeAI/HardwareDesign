# Basic 基本单元模块

## 概览

路径: `BaseCbb.basic`

基本门级单元库，可直接映射到标准工艺库单元。

---

## 基本门（组合逻辑）

| 类 | 说明 | 接口 |
|----|------|------|
| `Inv` | 反相器 | `in → out` |
| `Buf` | 缓冲器 | `in → out` |
| `And2` / `And3` | 2/3 输入与门 | `a, b, [c] → y` |
| `Nand2` / `Nand3` | 2/3 输入与非门 | `a, b, [c] → y` |
| `Or2` | 2 输入或门 | `a, b → y` |
| `Nor2` / `Nor3` | 2/3 输入或非门 | `a, b, [c] → y` |
| `Xor2` / `Xnor2` | 异或/同或门 | `a, b → y` |

## 组合选择

| 类 | 说明 |
|----|------|
| `Mux2` | 2 选 1 布尔多路选择器 |
| `Mux2N[T <: Data]` | 参数化类型 2 选 1 多路选择器 |
| `Dec2` | 2→4 译码器（one-hot 输出） |
| `Dec3` | 3→8 译码器 |

## 时序单元

| 类 | 说明 | 注意 |
|----|------|------|
| `DLatch` | D 锁存器 | 含组合反馈环，FIRRTL 不允许仿真 |
| `DFF` | D 触发器 | 使用显式 `io.clk` 端口 |
| `DFFAsyncRst` | 异步复位 D 触发器 | `AsyncReset` 类型 |
| `DFFSyncRst` | 同步复位 D 触发器 | `Reset` 类型 |

所有触发器均使用 `withClock` / `withClockAndReset`，需 Verilator 后端进行多时钟仿真。

## 算术/特殊单元

| 类 | 说明 |
|----|------|
| `HalfAdd` | 半加器 (`a + b → sum, cout`) |
| `FullAdd` | 全加器 (`a + b + cin → sum, cout`) |
| `SRLatch` | SR 锁存器（含组合反馈环） |
| `ClockGating` | 时钟门控（锁存器 + AND 避免毛刺） |
| `AOI22` | AND-OR-Invert: `!((a1 & a2) \| (b1 & b2))` |
| `AOI32` | AND-OR-Invert: `!((a1 & a2 & a3) \| (b1 & b2))` |

## 测试

`BasicCellsSpec` — 20 个测试覆盖所有组合门。
DLatch/SRLatch 因 FIRRTL `CheckCombLoops` 限制无法仿真。DFF/DFFAsyncRst/DFFSyncRst/ClockGating 因 Treadle 单时钟限制无法仿真，需 Verilator 后端。
