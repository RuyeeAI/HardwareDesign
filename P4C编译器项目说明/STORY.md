# STORY.md — P4→Chisel 编译器项目说明（技术评审）

## ① 用户意图对齐

- **目标受众**：组内技术评审会，听众是熟悉 Chisel/RTL 的硬件设计师与对编译器方法学感兴趣的同事。
- **核心目标**：讲完后听众相信——该编译器已按 XLS 方法论完成 M1~M5 + 切拍 + 运行时表项的完整落地，质量有门禁兜底（零回归 diff + 357/357 回归），值得在真实交换机项目中试用；并清楚后续路线（SDC 寄存器最小化、SRAM 表、lpm/ternary）。
- **PPT 长度**：14 页（3 个 Hero：封面 / 验证体系 / 结束页，占比 21%）。
- **视觉调性**：极简图纸白 / 克制 / 工程蓝图感 / 数据说话。
- **内容边界**：必讲——定位与 XLS 方法论、三层架构、M1~M5 里程碑、ActionDAG IR、切拍（加权分桶 + valid 链 + 编译指示）、运行时表项（指示/存储/参数化）、验证门禁、CLI 用法、与 XLS 对照差距、路线图。不讲——BaseCbb/GranularExtract/OSA 等其他子项目、代码逐行走读、测试用例细节。禁碰——未验证的性能宣称数字。

## ② 页面布局骨架

- **总页数 14，分 3 章**：
  - 第 1 章「概览与架构」：目录第 1 章 → 扉页 P3（编号 01，页码区间 P4–P6）
  - 第 2 章「关键机制」：目录第 2 章 → 扉页 P7（编号 02，页码区间 P8–P10）
  - 第 3 章「质量与展望」：目录第 3 章 → 扉页 P11（编号 03，页码区间 P12–P13）
  - 目录 3 章 ↔ 3 个 section 扉页，一一对应 ✓
- **Hero 页**：P1（封面）、P12（验证体系·巨型数字）、P14（结束页）——3/14=21%，互不相邻 ✓
- **rhythm 曲线**：P1 peak → P2 valley → P3 transition → P4 valley → P5 peak → P6 valley → P7 transition → P8 valley → P9 peak → P10 valley → P11 transition → P12 peak → P13 valley → P14 peak。无连续 3 页 valley ✓
- **版式预算**：非对称 8/14=57%（P2 左标题+右内容、P4 非对称双栏、P5 左大图+右侧文字、P8 非对称双栏、P9 左标题+右内容、P10 非对称双栏、P12 巨型数字+洞察、P13 左标题+右内容）；对称 1 页（P6 图表+洞察）；section/cover/ending 自定义。「左大图+右侧文字」+「非对称双栏」合计 3/14=21% ≤40% ✓；N卡片横排 0 次 ✓；相邻页版式均不同 ✓

## ③ 页面大纲

| # | title | type | role | rhythm | layout | visual | visual_role | density | anti_pattern | description |
|---|-------|------|------|--------|--------|--------|-------------|---------|--------------|-------------|
| 1 | P4 → Chisel 编译器 | cover | hero | peak | 全幅图+骑线文字 | L1: 细线架构字符流（SVG，铺满右下） | anchor | 字数约30 / 图1 / 留白50% | 禁止堆砌装饰几何；禁止居中对称排版 | 一行副标讲清项目本质：把 P4 报文处理编译为固化 Chisel RTL，参考 Google XLS 方法论 |
| 2 | 本次评审覆盖什么 | catalog | supporting | valley | 左标题+右内容 | L3: 母版徽标 | evidence | 字数约150 / 图0 / 留白30% | 禁止三卡片横排预览 | 三章导览：概览与架构（是什么）/ 关键机制（怎么做的）/ 质量与展望（做得怎样） |
| 3 | 01 · 概览与架构 | section | transition | transition | 章节大字 | L1: 超大章节数字 01 | anchor | 字数约30 / 图0 / 留白55% | 禁止四卡片预览；禁止铺满正文段落 | 章节扉页：先对齐"这个工具是什么、长什么样" |
| 4 | 定位与方法论 | content | supporting | valley | 非对称双栏 60:40 | L2: XLS 流水线对照（SVG） | evidence | 字数约220 / 图1 / 留白28% | 禁止 50:50 等分双栏 | 目标语言只支持标准 P4-16+v1model；方法论照搬 XLS：IR 承载全部优化、后端只做翻译——这是保证生成代码可读、可裁剪的前提 |
| 5 | 总体架构 | content | supporting | peak | 左大图+右侧文字 | L1: 五级架构图（SVG，占左60%） | anchor | 字数约180 / 图1 / 留白25% | 禁止把架构图缩成 200×70 角标 | 前端→ActionDAG IR→优化+调度→Chisel 后端→FIRRTL/Verilog；调度是 2026-09 新增环节，切拍能力由此进入 |
| 6 | 里程碑：M1→M5 一次讲完 | content | supporting | valley | 图表+洞察 | L1: 时间轴（SVG） | evidence | 字数约160 / 图1 / 留白30% | 禁止 N 卡片横排装里程碑 | 三天完成 M1 组合 action→M5 管线组装全落地，demo1~7 全部通过 chiseltest——说明子集路线图节奏可控，每步都有端到端验收 |
| 7 | 02 · 关键机制 | section | transition | transition | 章节大字 | L1: 超大章节数字 02 | anchor | 字数约30 / 图0 / 留白55% | 禁止铺满正文段落 | 章节扉页：三个最有含金量的机制设计 |
| 8 | 核心 IR：ActionDAG | content | supporting | valley | 非对称双栏 60:40 | L2: 节点类型与 pass 链（SVG） | evidence | 字数约230 / 图1 / 留白26% | 禁止等宽卡片横排 | 节点式位向量数据流图 + Sink 副作用（写 header/寄存器/计数器）；宽度不匹配显式报错而非静默截断——吸取 ParserCore 教训；IR 是全部优化的承载者 |
| 9 | 路径切拍：多拍流水化 | content | supporting | peak | 左标题+右内容 | L2: sV 延迟链示意（SVG） | evidence | 字数约240 / 图1 / 留白26% | 禁止把 valid 链公式写成代码块长串 | 加权分桶（RegRead 权重 2）+ StagedEmitter 平行发射；关键教训：valid 链必须 RegNext 纯延迟线，RegEnable 会永久锁高——此坑已固化为恒 1 拍脉宽回归断言；`// p4c: stages=N` 声明级指示 |
| 10 | 运行时表项：表从编译期到运行时 | content | supporting | valley | 非对称双栏 60:40 | L2: 条目编码布局（SVG） | evidence | 字数约230 / 图1 / 留白26% | 禁止 50:50 等分 | `// p4c: table X runtime`；条目打包 valid/actionId/参数/key，写后下一条查找可见；action 参数运行时化=Const 换成存储切片，IR 零改动；静态模式逐字节零回归 |
| 11 | 03 · 质量与展望 | section | transition | transition | 章节大字 | L1: 超大章节数字 03 | anchor | 字数约30 / 图0 / 留白55% | 禁止铺满正文段落 | 章节扉页：怎么证明它是对的、接下来去哪 |
| 12 | 验证门禁 | content | hero | peak | 巨型数字+洞察 | L1: 锚点数字 357/357（≥72px） | anchor | 字数约180 / 图0 / 留白40% | 禁止把核心数字塞进图表卡角落；禁止 L3 顶替 L1 | 357/357 全仓回归 + 76/76 编译器套件 + 15 个 demo/spec；N=1 模式与 git 基线逐字节 diff 为空——零回归不是口号而是机器门禁 |
| 13 | 工具链与路线 | content | supporting | valley | 左标题+右内容 | L2: CLI 用法示例 | evidence | 字数约220 / 图1 / 留白28% | 禁止长代码块铺满右栏 | 用法：sbt 自动生成 / P4cMain --stages N / P4C_STAGED_STAGES；路线：lpm/ternary key、BaseCbb SRAM 表、SDC/min-cut 寄存器最小化、FDO 延迟闭环 |
| 14 | 结束页 | ending | hero | peak | 居中金句 | L3: 母版徽标 | atmosphere | 字数约25 / 图0 / 留白55% | 禁止添加行动按钮堆砌 | 金句：IR 是核心资产——调度、优化、验证都发生在同一张图上；落款 HardwareDesign · 2026-09 |
