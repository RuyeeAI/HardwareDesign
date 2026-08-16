# data/ — 数据结构与基础基类

> 路径：`src/main/scala/BaseCbb/data/`　包：`BaseCbb.data`　文件 3 个：GenBundle / HeterogeneousBag / RecordMap

---

## 1. GenBundle.scala — 全库共享基类

> 从已删除的 `utils/GeneratorLib.scala` 恢复，FPP/*、memory、fifo、arbiter 均依赖。

```scala
class GenModule extends Module                       // 通用模块基类（空壳）
case class fldAttr(Desc: String, ResetValue: Long = 0L, ExpandArr: Boolean = false)
class GenBundle extends Bundle { var Attr: Map[Data, fldAttr] = Map() }
```
- `fldAttr`：字段属性注解（描述/复位值/展开数组标志）。
- ⚠ 字段名 `Desc` 大写开头，违反 Scala 命名惯例（建议 `desc`，会牵动引用方）。
- **注意**：`RegCbb.dsl.RegBundle`（v2 自包含 Bundle 式寄存器定义基类）与 `GenBundle` 功能类似但独立实现，
  仅在"Bundle + 逐字段注解"这一模式上重叠；RegCbb 不依赖本文件。

## 2. HeterogeneousBag.scala — 异构元素包

```scala
final case class HeterogeneousBag[T <: Data](elts: Seq[T]) extends Record with IndexedSeq[T]
```
- 字段名 = **数字索引字符串**（`"0".."n-1"`），保序；`apply(Int)` 访问。
- 覆写 `hashCode/equals` 委托 `Record`（防止 IndexedSeq 相等/哈希破坏 Chisel 图结构）。
- `fromNode(elts: Seq[(D,E)])`：只取类型克隆构造。

## 3. RecordMap.scala — 命名异构 Record

```scala
final class RecordMap[T <: Data](eltMap: ListMap[String, T]) extends Record
```
- 构造时 `requireIsChiselType` 校验；`apply(Int)`（O(n)）/ `apply(String)`（O(1)）/ `size` / `data`。
- 工厂：`apply(eltMap)` / `apply(elements: (String, T)*)`。

## 4. 两者关系与建议

| | HeterogeneousBag | RecordMap |
|---|---|---|
| 字段名 | 数字索引串 | 任意 String |
| 访问 | IndexedSeq 语义 + apply(Int) | apply(Int)（O(n)）/ apply(String) |
| 类型检查 | 无 | requireIsChiselType |
| 相等/哈希 | 覆写防破坏 | Record 默认 |

- **建议**：保留两个 API（IndexedSeq 视图 vs 命名 Map 是不同使用习惯），但让 `HeterogeneousBag` 内部委托
  `RecordMap` 消除底层重复，或至少补上 `requireIsChiselType` 检查。
