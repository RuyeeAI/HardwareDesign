# P4 → Chisel 编译工具设计文档

版本：v0.4（2026-09-06）
状态：M1~M5、路径切拍、运行时表项、XLS 对齐增补（签名导出 / clock 约束调度 / 延迟模型外置 / 寄存器合并 / 交叉引擎 fuzzer / 优化 pass）全部实现并通过测试（全仓 407/407，P4C 123/123）。
使用方法见 `docs/P4toChisel_使用指南.md`；增量细节见 `docs/P4toChisel_切拍_增量设计.md`、`docs/P4toChisel_运行时表项_增量设计.md`。

## 1. 目标与定位

将标准 P4-16 描述的交换机报文处理程序编译为固化的 Chisel 源码，最终经 FIRRTL 工具链生成 Verilog。

- **目标语言**：仅支持标准 P4-16 + v1model 的实用子集。不支持 TNA/PSA 等厂商架构。
- **方法论**：参考 Google XLS——源语言 → 核心数据流 IR → 优化 pass → 调度 → 后端。IR 承载全部优化，后端只做翻译。
- **产出形态**：生成参数化 Chisel（Scala）源码（包名 `p4cgen`），可读、可手工裁剪、可与手写模块（如本仓库 `FPP/Parser`）混用；另产出机器可读的签名/调度 JSON（供上位机与回归工具消费）。
- **验证方法论**：对标 XLS 的交叉引擎思路——IR 解释器（黄金）与生成 RTL（被测）对同一程序随机激励比对，不依赖 BMv2。

## 2. 范围与子集

| 能力 | 支持范围 | 说明 |
|------|---------|------|
| action | 位串操作（`++` `[]` 切片）、算术/比较/移位/逻辑、`set` 赋值 | `*` `/` `%` 不支持 |
| table | exact 匹配；**静态融合表**（const entries）与**运行时表**（编译指示启用） | lpm/ternary 不支持（见增量 PRD P1-1） |
| parser | 状态机（`extract`/`transition`/`select`）→ FSM Module | 固定字节偏移、512-bit 报文窗口 |
| 状态单元 | `Register` / `Counter` extern（`io.valid` 门控写、`ex_<name>` 观测口） | meters/checksum 未做 |
| 管线 | parser + control 组装为 `<Prefix>Top`（一次性 fire 锁存 + outValid） | 多 parser/control 组合未做 |
| 控制流 | apply 顺序块（action 调用 / 赋值 / extern 调用 / 表 apply） | `if hit` / `switch` 未做 |

始终不支持：控制平面计算、通用递归、指针、动态内存。

## 3. 总体架构（as-built）

```
P4 源码（P4-16/v1model 子集）
    │  ① 前端：Preprocess（注释/#行剥除，classify 状态机）→ Lexer → Parser（递归下降）
    │     编译指示扫描：Directive.scan（stages=N / table X runtime [size=N]，紧邻性 + 认领/告警）
    ▼
P4 AST（Ast.scala；声明级指示记录在 stagesOpt / TableDecl.isRuntime+runtimeSize）
    │  ② IR 构建：IrBuilder（宽度推断 + 显式 fit，绝不静默截断）
    ▼
核心 IR：ActionDAG（Ir.scala）
    ├─ Node：Const/InputRef/Zext/Trunc/Bin/Slice/Cat/Mux/Not/RegRead
    ├─ Sink：OutputWrite / RegWrite / CounterAdd（恒定末级约定）
    └─ stages: Map[NodeId,Int]（调度标注，空 = 全组合）
    │  ③ 优化 pass（Passes.runAll = dce ∘ cse ∘ simplify ∘ constFold）
    │  ④ 调度（Scheduler，必须在 runAll 之后、CSE 不得再跑）
    │     延迟模型 DelayModel（weighted/unit/外部 JSON）+ 预算解算（stages 指示 > clock 模式 > 全局 N）
    ▼
已调度的 ActionDAG
    │  ⑤ Chisel 后端（ChiselBackend）：Emitter（N=1）/ StagedEmitter（切拍）双路
    │     静态融合表 / 运行时表 / parser FSM / Top 组装 / 文件头协议注释
    ▼
生成的 Chisel 源码（p4cgen）+ 签名 JSON（Signature）→ FIRRTL → Verilog
```

旁路产物：
- **签名/调度 JSON**（`Signature.scala`）：模块端口（Bundle 展平点分路径）、表清单与布局、extern、各 DAG 节点→流水级映射（对标 XLS signature/schedule textproto）。
- **IR 解释器**（`Interp.scala`）：ActionDAG 直接求值，作为交叉引擎验证的黄金引擎（对标 XLS IR interpreter）。

前端为 Scala 自研（快速迭代、无 C++ 工具链依赖）；后续如需完整 P4 兼容性再评估对接 p4c。

## 4. 前端

### 4.1 词法/预处理（Lexer.scala）

- `Preprocess(src)`：剥除注释与 `#` 行，**换行数不变**（块注释内换行保留为空白行）——词法/语法行号与原始行号一一对应，是指示紧邻性匹配的基础。
- `Preprocess.classify(src): Array[Byte]`（private[P4C]）：共享注释状态机（0=Code/1=行注释/2=块注释），`Directive.scan` 与 `Preprocess.apply` 共用同一实现——防止"块注释内指示样文本被当真"类 Bug（教训：注释识别状态机只写一份）。
- `Lexer.tokenize`：带宽字面量 `16w0x0800`、最长匹配符号表。P4 子集无字符串字面量（`"` 直接词法报错）。

### 4.2 语法（Parser.scala）

递归下降；声明解析时按**紧邻性**（指示行与声明行之间仅允许空白行）认领编译指示。两类指示正则互斥、map 独立、共享 `claimed` 集合与 `adjacentDirective[T]` 泛型辅助：

| 指示 | 作用对象 | 语义 |
|------|---------|------|
| `// p4c: stages=N` | 紧邻的 control/parser/action | 声明级切拍预算覆盖（parser 仅记录不生效） |
| `// p4c: table <表名> runtime [size=N]` | 紧邻的 table | 启用运行时表；表名是冗余校验（贴错 → P4Error）；size 缺省 4 |

不满足紧邻性的指示**忽略并告警**（宽容策略）；块注释内的指示样文本被抑制（专属告警）。

### 4.3 AST（Ast.scala）

HeaderType/StructType/ActionDecl/Param/TableDecl（`isRuntime`/`runtimeSize`）/TableEntry/ControlDecl（`stagesOpt`）/ExternInst/ParserDecl。宽度过推断在 IrBuilder 完成，AST 只携带语法宽度。

## 5. 核心 IR：ActionDAG（Ir.scala）

XLS 式 node-based 位向量数据流图。不变量：

- 每节点 `width` 即生成 Chisel `UInt` 的精确宽度；
- Bin 两操作数经 `fit`（Zext/Trunc 显式节点）归一到同宽——**绝不静默截断**（ParserCore 教训）；
- NodeId 升序即拓扑序（Builder 追加构造）；
- Sink 恒定末级（切拍约定），保证 RegRead/RegWrite 读旧值语义结构上成立（D3 断言守护）。

节点：`Const / InputRef(path) / Zext / Trunc / Bin(op) / Slice / Cat（先声明在高位）/ Mux / Not / RegRead`。
Sink：`OutputWrite(path) / RegWrite / CounterAdd`。

### 5.1 优化 pass（Passes，进 `runAll` 链）

顺序：`constFold → simplify → cse → dce`（simplify 产出的新结构交由 CSE 去重）。

| pass | 内容 |
|------|------|
| constFold | Const 参与的纯运算直接求值；0 侧吸收（And/Or/Xor/Add）；Mux 条件常量 / t==f 合并 |
| simplify | ① slice-of-concat 直切（Slice(Cat) → 直接切 parts）②布尔恒等（And(x,全1)=x、Or(x,全1)=全1、Xor(x,全1)=Not(x)、Not(Not(x))=x、零位移）③自反消除（Sub/Xor(x,x)=0、And/Or(x,x)=x） |
| cse | 结构等价节点 hash-cons 合并（**调度后禁止**——会跨级合并） |
| dce | 从 outputs 可达才保留 |

## 6. 调度（SchedulePass.scala）

### 6.1 延迟模型（DelayModel.scala，X6/X7）

`trait DelayModel { def weight(n): Double }`——节点延迟代价，**口径 = 相对 ND2（二输入 NAND）门延迟的倍数，ND2 一级 = 1.0**（X7 起统一为小数口径；0 = 纯布线）。内置：

- `weighted`（默认，E1 表）：Cat/Slice/Zext/Trunc/Not/Const/InputRef = 0；Bin/Mux = 1；RegRead = 2；
- `unit`（XLS unit 对标）：叶子 0，其余 1；
- `logiceffort`（X7，Logic Effort）：单门 op 按 `d = (g·h+p)/(g_ND2·h+p_ND2)`（参考扇出 h=1）：INV 0.6、NAND/NOR+INV（And/Or）1.6、XOR2 3.0、2:1 mux 1.2；复合 op 按门网络展开——Add/Sub = w（行波进位链上界）、Shl/Shr = 1.2·log2(w)（桶形移位）、Eq/Neq = 3.0+1.6·log2(w)、比较器 = 3.0+2.4·log2(w)、RegRead = 1.2·log2(size)（读 mux 树）。高估方向保守（真实综合可构建 CLA 等）；
- 外部 JSON：op 名→权重（允许小数），可按 `Bin(Add)` 按运算符细分，缺项 P4Error。工艺特征化模型实现 trait 即可接入。

已知简化：忽略扇出负载（h=1）与线电容；单节点视为原子不可跨级切分——16-bit 加法器（16 级）在 clock<16 时如实报不可行。

### 6.2 调度算法

加权深度 `wd(n) = weight(n) + max(wd(操作数))`，`W = max wd`；`n = min(budget, W+1)`；`stage(x) = min(n-1, wd(x)·n/(W+1))`（单调 ⇒ 操作数组恒 ≤ 使用组）。W=0（全布线 DAG）不调度。

### 6.3 预算解算（优先级从高到低）

1. 声明级 `// p4c: stages=N` 指示；
2. **clock 模式**（X2，对标 XLS 两阶段调度）：给定每级组合延迟上限 `--clock W`，逐 DAG 求**最小可行级数**——`minFeasibleStages` 线性扫描（规避分桶非严格单调假设），下界 `minClock` = 最大单节点权重（单节点不可切分），低于下界报 P4Error 并附最小可行周期（minimize_clock_on_failure 等价）；级延迟 `stageDelays(k) = max(arrival∈k) − start(k)`；
3. 全局固定 `--stages N`（sbt `P4C_STAGES`）。

运行时表在切拍（stages>1 或 clock 模式）下显式 P4Error（Q4 裁定：本期仅 N=1，不做静默降级）。

## 7. Chisel 后端（ChiselBackend.scala）

### 7.1 双路发射

- **Emitter（N=1）**：DAG → 组合表达式；公共子表达式落 val（引用计数）；stateful 写按 fireCond 包 `when`。
- **StagedEmitter（切拍）**：valid 链 `sV_k = RegNext(sV_{k-1}, false.B)`（**纯延迟线**——RegEnable 会永久锁高，历史教训）；跨级节点边界寄存 `RegEnable(expr, 0.U, sV_k)`；Sink 末级提交。**跨级同值边界寄存器合并**（X3，对标 XLS register_merge_strategy=identity）：StagedShared.regCache 按 (sV 名, 表达式文本, 位宽) 共享 val。

时序契约：io.valid 单拍脉冲、发起间隔 ≥ N、Top 一次性 fire 天然满足。

### 7.2 静态融合表（emitStaticTable）

`const entries` 烘焙为 `hit_i = key === const` 比较 + 字段 `MuxCase`（声明序优先）；每条目一份常量参数 DAG。

### 7.3 运行时表（emitRuntimeTable，D1~D5）

- 存储：`rt_<name> = RegInit(VecInit(Seq.fill(size)(0.U(entryW.W))))`，上电全 0 ⇒ 空表全 miss；
- 条目布局（MSB→LSB）：`valid(1) | actionId(actW) | args(argW) | key(keyBits)`，单字打包；
- 写口：`tbl_<name>_we/waddr/wdata`，时钟沿单字原子提交，`waddr < size` 越界守卫；写 valid=0 即删除；
- 查找：逐条目 `valid && key 匹配` → `PriorityMux` 低地址优先（与静态表声明序同构）；组合读、零拍延迟，不触碰切拍 valid 链；
- action 参数运行时化：每 **action** 一份参数化 DAG（形参绑定为合成 `__rtarg` InputRef，发射时映射到 args 切片），按 actionId 选通；default 行编译期固定（lowerEntry 常量路径）；
- 同 control 内静态/运行时表可共存；协议/布局/可见性（"写拍当拍查找见旧值，下一拍起新值，绝不撕裂"）写入生成文件头注释。

### 7.4 Top 与文件头

parser + control 共存时发射 `<Prefix>Top`：`fire = parser.done && !parser.error && !fired` 一次性锁存；运行时表写口透出。运行时表存在时文件头追加协议注释（写接口时序 / 位布局表 / 表深与位宽回显 / 可见性语义 / 上电为空）。

## 8. 验证策略（as-built）

1. **单元级**：pass 逐条单测（IrPassSpec，含 simplify 属性检查：优化前后 DAG 对随机输入求值一致）；Scheduler/Directive/DelayModel 纯 Scala 单测。
2. **交叉引擎 fuzzer**（X4，对标 XLS fuzzer）：黄金引擎（AST → IrBuilder → **Interp** IR 求值）vs 生成 RTL（chiseltest 随机激励），固定 seed 每程序 20 轮 × demo1/demo2/demo7（含随机写运行时表项）。运算语义单一实现（`Interp.evalOp` 同时供 constFold 与解释器），杜绝两份漂移。
3. **等价性**：切拍 N=1 vs N=3/4 行为等价（StagedEquivalenceExtraSpec 等）；N=1 基线逐字节 diff 门禁（`diff -r generated/p4c <基线>`，基线 /tmp/p4c-baseline-x5，12 文件）。
4. **回归**：`src/test/scala/P4C/` 20 suites / 123 tests；全仓 63 suites / 407 tests。

## 9. 里程碑状态

| 里程碑 | 状态 |
|--------|------|
| M1 Action / M2 静态表 / M3 parser FSM / M4 Register+Counter / M5 Top 组装 | ✅ demo1~5 |
| 路径切拍（Scheduler + StagedEmitter，声明级指示） | ✅ demo6-deepchain、staged/ 变体 |
| 加权延时模型 + 编译指示 | ✅（已并入 X6 外置模型） |
| 运行时可配置表项（指示 + 写口 + 参数化 DAG 发射 + 八条测试矩阵） | ✅ demo7 |
| XLS 对齐：签名导出 / clock 约束调度 / 延迟模型外置 / 寄存器合并 / 交叉 fuzzer / simplify pass | ✅ X1~X5（2026-09-06） |

## 10. 与原设计（v0.1）的偏差

- IR 实际形态为 node-based ActionDAG + Sink（Param→InputRef、FieldWrite→OutputWrite）；ParseGraph/MatchPlan 未单独建 IR，由 AST + 后端发射承担。
- 表资源映射：`BaseCbb` SRAM / TCAM 路线未接（运行时表当前 Vec[Reg] 组合读；SRAM 同步读 1 拍与切拍 valid 链的衔接是开放问题）。
- 验证未接 BMv2，以交叉引擎 fuzzer 替代（口径见 §8.2）。
- 生成代码风格：规整模板 + 文件头协议注释（稳定性优先）。

## 11. 开放问题（后续立项）

1. **SDC-LP 调度**：XLS 的寄存器最小化是 LP 精确解；P4C 目前均匀分桶 + 同值合并，需引入 LP/最小割求解器。
2. **SMT 形式等价**：IR↔RTL 逻辑等价证明（Z3/Bitwuzla）。
3. **fuzzer 扩展**：extern 顺序语义、parser/Top 的交叉验证。
4. **proc/通道抽象**：通用流式进程（当前 parser FSM 是特化产物）。
5. **综合/PPA 闭环**：Yosys/OpenSTA 反馈延迟模型（XLS FDO 形态）。
6. **SRAM/TCAM 资源映射**与 lpm/ternary 表。
7. `if hit` / `switch` 控制流、递归 parser 有界展开。
