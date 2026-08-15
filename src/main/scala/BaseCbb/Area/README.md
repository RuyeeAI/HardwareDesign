# Area 面积估算模块

## 概览

路径: `BaseCbb.Area`

提供基于工艺参数的面积估算模型，用于设计初期面积评估。

---

## ProcessConfiguration — 工艺配置

预设 T7、S12、S7 三种工艺的面积参数:

```scala
ProcessConfiguration.T7.mux2_area       // MUX2 单元面积
ProcessConfiguration.T7.ff_area        // DFF 单元面积
ProcessConfiguration.T7.nd2_area        // NAND2 单元面积
ProcessConfiguration.T7.logic_uti       // 组合逻辑利用率
ProcessConfiguration.T7.mem_uti        // 存储器利用率
ProcessConfiguration.T7.comb_incr_syn2pr // 综合→布局布线膨胀系数
```

导出参数 (`pd_*`) 为利用率调整后的面积:
- `pd_mux2_area` / `pd_ff_area` / `pd_nd2_area`

---

## GenArea — 面积统计

```scala
case class GenArea(ff_num: Int, comb_area: Double = 0, mem_area: Double = 0)
```

| 属性 | 计算 |
|------|------|
| `ff_area` | `ff_num × pd_ff_area` |
| `total_area` | `ff_area + comb_area + mem_area` |
