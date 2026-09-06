# P4 → Chisel 编译工具设计文档

版本：v0.3（2026-09-05）
状态：M1~M5 已实现并通过 chiseltest 验证；路径切拍（Action/Control 调度）已实现（详见 `docs/P4toChisel_切拍_增量设计.md`）

## 1. 目标与定位

将标准 P4-16 描述的交换机报文处理程序编译为固化的 Chisel 源码，最终经 FIRRTL 工具链生成 Verilog。

- **目标语言**：仅支持标准 P4-16 + v1model 架构。不支持 TNA/PSA 等厂商架构。
- **方法论**：参考 Google XLS——源语言 → 核心数据流 IR → 优化 pass → 调度 → 后端。IR 承载全部优化，后端只做翻译。
- **产出形态**：生成参数化 Chisel（Scala）源码，可读、可手工裁剪、可与手写模块（如本仓库 `FPP/Parser`）混用，而非直接吐低级中间表示。

## 2. 范围与子集

完整 P4 太大，按阶段收敛子集：

| 阶段 | 支持范围 |
|------|---------|
| M1（Action MVP） | action 函数体：位串操作（`<<` `>>` `\|\|` 位切片）、算术、比较、`set` 赋值、常量折叠 |
| M2 | 完整 Match-Action：`table` 定义（exact/ternary/lpm）、key 构建、action 选择与参数传递 |
| M3 | 报文解析：`parser` 状态机（`extract`/`transition`/`select`）、header 类型定义、`deparser` |
| M4 | v1model 状态单元：`registers`、`counters`、`meters`、`direct_registers`、checksum 计算 extern |
| M5 | 完整 `ingress`/`egress` 管线组装、多表串接、hit/miss 分支 |

始终不支持：控制平面计算（`if` 包含运行时不可判定项按表处理）、通用递归、指针、动态内存。

## 3. 总体架构

```
P4 源码 (P4-16/v1model)
    │  ① 前端：词法/语法分析 → 符号表 → 类型检查
    ▼
P4 AST + 语义信息
    │  ② IR 构建（按三部分分别降级）
    ▼
核心 IR（三层）
    ├─ ParseGraph   —— 报文解析状态图
    ├─ MatchPlan    —— 匹配计划（表、key、action 引用）
    └─ ActionDAG    —— 位向量数据流图（XLS 式）
    │  ③ 优化与调度（pass 链）
    ▼
已调度的 IR（每个节点标注：组合 / 第 N 拍 / 资源类型）
    │  ④ Chisel 后端：按层翻译为 Scala 源码
    ▼
生成的 Chisel 模块 → FIRRTL → Verilog
```

前端先用 Scala 自研（快速迭代、无 C++ 工具链依赖）；若后续需要完整 P4 兼容性，再评估对接 p4c 前端。

## 4. 核心 IR 设计

三个子 IR 共享同一套类型系统（位宽向量 + header 实例图），统一由 IRBuilder 产出。

### 4.1 ActionDAG（最先实现）

节点类型（对齐 XLS 的 node-based dataflow IR）：

```scala
sealed trait ActionNode {
  val id: NodeId
  val width: Int          // 推断出的位宽
  val sched: Schedule     // Combinational | Stage(n)
}
case class Const(v: BigInt, width: Int)
case class Param(name: String, width: Int)            // action 参数 / match key 字段
case class FieldRef(header: String, field: String)    // 读 header 字段
case class Concat(parts: Seq[NodeId])
case class Slice(src: NodeId, hi: Int, lo: Int)
case class BinOp(op: Op, lhs: NodeId, rhs: NodeId)    // Add/Sub/And/Or/Xor/Shl/Shr/Lt/Eq...
case class Mux(cond: NodeId, t: NodeId, f: NodeId)    // 有限三元
case class FieldWrite(header: String, field: String, value: NodeId)  // set_* 副作用
```

优化 pass（M1 必备）：常量折叠、宽度归一（零扩展/截断显式化，吸取 ParserCore 教训：宽度不匹配必须报错而非静默截断）、CSE、死代码消除、`Slice/Concat` 对合并。

调度（XLS 核心思想）：默认全组合；用户可指定目标频率 → pass 把超过预算的路径切拍（插入寄存器节点），ActionDAG 拓扑序天然支持。

### 4.2 ParseGraph

- 节点 = parser 状态；边 = `transition(select(...))` 谓词。
- 分析：状态可到达性、每状态累计提取偏移、循环检测（v1model parser 允许有界循环 → 展开或报错）。
- 实现策略二选一（可按路径复杂度自动选择）：
  - **FSM**：通用但慢，适合深路径；
  - **流水线**：每级提取固定偏移窗口，与 `FPP/Parser` 手写结构一致，作黄金对照。

### 4.3 MatchPlan

- 每表：key 类型（exact/lpm/ternary）→ 资源映射（哈希+SRAM / TCAM+优先级编码）→ 匹配流水级。
- action 参数组装是 ActionDAG 的实例化；hit/miss 是有限 Mux。
- 表存储走 `BaseCbb/memory` 封装（Sp/TpMemoryWrap 系列）。

## 5. Chisel 后端

- 输入：已调度的 IR。输出：Scala 源码文件（每管线一个顶层 `Module`）。
- ActionDAG → 组合表达式 / 寄存器链；FieldWrite → header Bundle 更新。
- MatchPlan → 参数化的 `MatchTable` 模块（key 宽度、表深、资源类型作构造参数）。
- ParseGraph → FSM 或 Pipeline Parser（复用 `FPP/Parser/PipelineStage` 模式）。
- header 类型 → Chisel `Bundle`；寄存器配置可对接 `RegCbb` 框架。

## 6. 验证策略

1. **单元级**：每个 pass 的 IR 快照测试（golden JSON）。
2. **等价性**：同一 P4 程序跑 BMv2 behavioral model（行为参考）vs 生成 RTL 的 chiseltest 激励回放，报文级 diff。
3. **黄金对照**：生成 Parser vs 手写 `FPP/Parser` 在相同报文集下的输出比对。
4. **回归**：`src/test` 加入 `P4C/` 测试套，与现有 43 suites 并行。

## 7. 里程碑

| 里程碑 | 交付物 | 验收 | 状态 |
|--------|--------|------|------|
| M1 | 前端(词法/语法/子集) + ActionDAG IR + 3 个优化 pass + 组合 Chisel 后端 | 一个含位拼接/算术的 P4 action，生成模块通过 chiseltest，与手写参考一致 | ✅ demo1 |
| M2 | MatchPlan + 表资源映射 + 调度(切拍) | exact 表 Match-Action 单元，BMv2 对照通过 | ✅ demo2（exact 表静态融合；BMv2 对照未接）。**切拍已实现**（2026-09-05：Scheduler 均匀分桶 + StagedEmitter，N=1 零回归，见 `docs/P4toChisel_切拍_增量设计.md`） |
| M3 | ParseGraph + FSM/流水线 Parser 生成 | 与 FPP/Parser 黄金对照 | ✅ demo3（FSM 路线；流水线 Parser 与黄金对照未做） |
| M4 | v1model 状态单元 | Register/Counter demo 通过 | ✅ demo4（meters/checksum 未做） |
| M5 | 完整管线组装 | parser→control 端到端，一次性触发+输出锁存 | ✅ demo5（L2/L3 转发示例未做） |

## 8. 实现落地说明（v0.2）

- **代码位置**：编译器 `src/main/scala/P4C/`（Ast/Lexer/Parser/Ir/IrBuilder/Passes/ChiselBackend/Generate），demo `p4/demos/demo1~5.p4`，生成代码 `generated/p4c/`（包名 `p4cgen`），测试 `src/test/scala/P4C/`。
- **sbt 集成**：`project/p4c.sbt` 元构建钩子（编译器源码 Scala 2.12 兼容、零 Chisel 依赖）+ 根 build.sbt `p4Generate` sourceGenerator，`.p4` 改动自动重新生成。
- **与原设计的偏差**：
  - 表当前为 `const entries` 静态融合（MuxCase 烘焙进组合逻辑），运行时可配置表/`BaseCbb` SRAM 资源映射为后续工作；
  - Parser 仅 FSM 路线；`ParseGraph` 概念由 AST 直接承载，未单独建 IR；
  - IR 实际形态为 node-based `ActionDAG` + `Sink`（OutputWrite/RegWrite/CounterAdd），与 4.1 草案字段命名有出入（Param→InputRef、FieldWrite→OutputWrite 等）。

## 9. 开放问题

1. P4 的 `apply` 顺序控制流（顺序块 + `if hit` + `switch`）映射为多级 MatchPlan 串接，`switch` 的 action 携带语义需要单独 pass 展开。
2. 递归 parser（MPLS/VXLAN 变长头链）的有界展开深度策略。
3. TCAM 面积估算是否接入 `BaseCbb/Area`。
4. 生成代码风格：贴近手写 Chisel（可读性优先）还是规整模板（稳定性优先）——倾向后者。
