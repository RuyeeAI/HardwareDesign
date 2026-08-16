# Clos/ — Benes 置换网络

> 路径：`src/main/scala/BaseCbb/Clos/BenesClos.scala`（111 行）　包：`BaseCbb.Clos`
> 依赖：`BaseCbb.misc.{Seq2Vec, SubVec}`

---

## 1. BenesClos2x2 — 2×2 交叉单元

```scala
class BenesClos2x2[T <: Data](dt: T)
// sel: Bool（0=直通，1=交叉）；in/out: Vec(2, dt)
```
- 纯组合：`out := Mux(sel, Seq2Vec(in.reverse), in)`。

## 2. BenesClos — N×N 网络顶层

```scala
class BenesClos[T <: Data](dt: T, Num: Int)
// sel: Vec(CfgSize(Num), Bool)（配置位）；in/out: Vec(Num, dt)
```
- `io.out := Benes.ClosNxN(io.sel, io.in)` 递归构建；companion `apply(s, in): Vec[T]`。
- ⚠ 未校验 `sel` 长度 == `CfgSize(Num)`，不匹配时 SubVec 切片越界/错位（Clos2x2 有 require，顶层没有）。

## 3. Benes — 递归算法

- `CfgSize(len)`：配置位总数。递归 `len>=3 → CfgSize(⌊len/2⌋)+CfgSize(⌈len/2⌉)+⌊len/2⌋*2`；`len==2 → 1`；`len<2 → 0`。
  实测：N=2→1、N=3→3、N=4→6、N=5→8。
- `ClosNxN(s, in)`：递归分割——第一级 `upperHalf` 个 2×2 吃输入对 → 上/下两个子网（递归）→ 末级 `upperHalf` 个 2×2 合并；
  奇数 N 时多余输入直连。配置位布局：第一级 → 上子网 → 下子网 → 末级，与 CfgSize 递归一致。

## 4. 设计注意

1. **无校验**：顶层 `sel` 长度不检查（建议 `require(sel.length == CfgSize(Num))`）。
2. 空 Vec 时 `apply` 用 `in.head.cloneType` 会崩溃。
3. `math.floor/ceil` 可简化为整数除法；`upperHalf.toInt` 对 Int 冗余。
4. 注释残缺（Scaladoc 未写完）、陈旧注释（`firstStageCfg` 等）。
5. 无专门测试文件。
