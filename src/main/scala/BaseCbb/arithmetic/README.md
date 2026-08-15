# Arithmetic 算术单元模块

## 概览

路径: `BaseCbb.arithmetic`

提供加法器、减法器、比较器、乘法器和移位器的 RTL 实现。

---

## 加法器

### RippleCarryAdder — 逐位进位加法器

```scala
class RippleCarryAdder(width: Int = 32) extends Module
```

N 个 `FullAdd` 链式级联。O(N) 延迟。

| 信号 | 方向 | 类型 | 说明 |
|------|------|------|------|
| `a` | Input | UInt(width.W) | 操作数 A |
| `b` | Input | UInt(width.W) | 操作数 B |
| `cin` | Input | Bool | 进位输入 |
| `sum` | Output | UInt(width.W) | 和 |
| `cout` | Output | Bool | 进位输出 |

### CarrySelectAdder — 进位选择加法器

```scala
class CarrySelectAdder(width: Int = 32, blockSize: Int = 4) extends Module
```

分段并行计算 `carry=0` 和 `carry=1` 两种结果，由实际进位选择。O(blockSize + N/blockSize) 延迟。接口同 `RippleCarryAdder`。

### Subtractor / AddSub

| 类 | 说明 |
|----|------|
| `Subtractor(width)` | 减法器 (`a + ~b + 1`)，含 `borrowOut` |
| `AddSub(width)` | 加法/减法可配置 (`sub` 信号选择) |

---

## Comparator — 比较器

```scala
class Comparator(width: Int = 32) extends Module
```

| 输出 | 说明 |
|------|------|
| `eq` | `a == b` |
| `gt` | `a > b` |
| `lt` | `a < b` |

---

## Multipler — 乘法器

```scala
class Multipler(widthA: Int = 16, widthB: Int = 16) extends Module
```

行为级乘法，输出宽度 = `widthA + widthB`。

---

## 移位器

| 类 | 说明 |
|----|------|
| `LeftShifter(width)` | 左移位 (`din << shamt`)，丢失高位 |
| `RightShifter(width, arithmetic)` | 右移位，`arithmetic=true` 时算术移位（符号扩展），否则逻辑右移 |
