# Sequential 时序单元模块

## 概览

路径: `BaseCbb.sequential`

提供寄存器、寄存器文件、计数器和时钟分频器等时序电路。所有模块使用显式时钟端口（`io.clk`），需 Verilator 后端进行仿真。

---

## 寄存器

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

| 类 | 读端口 | 写端口 |
|----|--------|--------|
| `RegFile1R1W(dataWidth, addrWidth)` | 1 | 1 |
| `RegFile2R1W(dataWidth, addrWidth)` | 2 | 1 |

---

## 计数器

### UpCounter — 递增计数器

```scala
class UpCounter(width: Int = 8) extends Module
```

上溢时 `carry = 1`。支持 `en` 和 `clear` 控制。

### ModNCounter — 模 N 计数器

```scala
class ModNCounter(mod: Int = 100) extends Module
```

上溢后回到 0，输出 `overflow` 脉冲。

---

## 时钟分频器

| 类 | 说明 |
|----|------|
| `ClkDiv2` | 2 分频（50% 占空比），输出 `Clock` |
| `ClkDivOdd(div)` | 奇数分频（50% 占空比），`div` 须为奇数 |
| `ClkDiv(div)` | 通用整数分频，输出 `Bool` |
| `ClockDivider2` | 2 分频 BlackBox（需 Verilog 实现） |
| `ClockDivider3` | 3 分频 BlackBox（需 Verilog 实现） |
| `Pow2ClockDivider(pow2)` | 2^pow2 次分频（链式 ClockDivider2） |

---

## FsmTemplate — FSM 模板

```scala
class FsmTemplate(stateNum: Int = 4) extends Module
```

| 信号 | 说明 |
|------|------|
| `start` / `doneCond` | 启动/完成条件 |
| `idle` / `busy` / `done` | 状态标志 |
| `currentState` | 当前状态编码 |

`FsmStates` 对象预定义 `sIDLE :: sBUSY :: sDONE :: Nil = Enum(3)`。
