# Clos 置换网络模块

## 概览

路径: `BaseCbb.Clos`

递归构建的 Benes 置换网络，支持任意 `Num × Num` 的输入输出排列。

---

## BenesClos — Benes 网络

```scala
class BenesClos[T <: Data](dt: T, Num: Int) extends Module
```

| 信号 | 方向 | 类型 | 说明 |
|------|------|------|------|
| `sel` | Input | Vec(Benes.CfgSize(Num), Bool) | 配置位 |
| `in` | Input | Vec(Num, dt) | 输入数据 |
| `out` | Output | Vec(Num, dt) | 排列后的输出 |

**配置位数量**: `Benes.CfgSize(Num)` — 递归计算。

## BenesClos2x2 — 2×2 交换单元

```scala
class BenesClos2x2[T <: Data](dt: T) extends Module
```

| `sel=0` | 直通: `out(0)=in(0)`, `out(1)=in(1)` |
| `sel=1` | 交叉: `out(0)=in(1)`, `out(1)=in(0)` |

## 配套对象

`Benes` 对象提供:
- `CfgSize(len)` — 计算 N-port Benes 网络所需配置位数
- `Clos2x2(sel, in)` — 例化 2×2 交换单元
- `ClosNxN(s, in)` — 递归构建完整 Benes 网络

`BenesClos` companion: `apply(s, in)` 工厂方法。
