# Area/ — 面积估算模型

> 路径：`src/main/scala/BaseCbb/Area/AreaCalc.scala`（40 行）　包：`BaseCbb.Area`

---

## 1. ProcessConfiguration — 工艺配置

```scala
object ProcessConfiguration   // process 硬编码 "T7"
```
| 成员 | 类型 | T7 值 | 含义 |
|------|------|-------|------|
| `mux2_area` | var | 0.15 | 2:1 MUX 单元面积 |
| `ff_area` | var | 0.36 | 触发器单元面积 |
| `nd2_area` | var | 0.0547 | NAND2 单元面积 |
| `logic_uti` | val | 0.4 | 逻辑利用率 |
| `xbar_uti` | val | 0.3 | 交叉开关利用率 |
| `mem_uti` / `tcam_uti` | val | 0.75 / 0.6 | **代码内未使用** |
| `comb_incr_syn2pr` | val | 1.4 | 综合→布局面积增量系数 |
| `pd_mux2_area` 等 | val | 换算后 | **设计后**（post-design）单位面积 |

- 换算公式：`pd_mux2_area = mux2_area * comb_incr_syn2pr / xbar_uti`；`pd_ff_area = ff_area / logic_uti`；`pd_nd2_area = nd2_area * comb_incr_syn2pr / logic_uti`。

## 2. GenArea — 面积估算结果

```scala
case class GenArea(ff_num: Int, comb_area: Double = 0, mem_area: Double = 0)
// ff_area = ff_num * pd_ff_area；total_area = ff_area + mem_area + comb_area
```
- 纯数据类；⚠ `GenArea.ff_area`（总 FF 面积）与 `ProcessConfiguration.ff_area`（单 FF 面积）**同名异义**。

## 3. 设计注意

1. `process` 硬编码 `val "T7"`，`S12`/`S7` 两个 if 分支**永不可达**（死分支）；README 提及的 `T7/S12/S7` 子对象 API 与实现不符。
2. `mem_uti`/`tcam_uti` 定义后无任何引用（死常量）。
3. 全局 `var` 可变状态；面积单位与 `pd_` 前缀含义无注释。
4. 无专门测试文件。
