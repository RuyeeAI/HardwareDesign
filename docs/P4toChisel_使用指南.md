# P4 → Chisel 编译工具使用指南

版本：v0.2（2026-09-06，对应设计文档 v0.4）
适用范围：`src/main/scala/P4C/` 编译器 + `p4/demos/` 生成管线 + `P4cMain` CLI。
设计背景与实现细节见 `docs/P4toChisel_设计文档.md`（v0.4）。

---

## 1. 环境与构建

- JDK 8+ / sbt（Scala 2.13.12）、Chisel 3.6.1、chiseltest 0.6.2。
- 编译器自身为纯 Scala（零 Chisel 依赖），经 `project/p4c.sbt` 元构建钩子进入 sbt meta-build。

```bash
sbt compile          # 自动编译 p4/demos/*.p4 → generated/p4c/（sourceGenerator，.p4 改动自动重生成）
sbt test             # 全仓回归（当前 407/407）
sbt "testOnly P4C.*" # 仅 P4C 套件（当前 123/123）
```

## 2. 目录速览

```
src/main/scala/P4C/    编译器（Ast/Lexer/Parser/Directive/Ir/IrBuilder/
                       SchedulePass/DelayModel/Interp/Signature/ChiselBackend/Generate）
src/test/scala/P4C/    测试（前端/IR/调度/指示/各 demo 端到端/交叉 fuzzer/签名）
p4/demos/              demo 源（demo1~8），改动自动重生成
p4/demos/staged/       切拍变体 demo（类名 +Staged 后缀，拍数由 P4C_STAGED_STAGES 决定）
generated/p4c/         生成 Chisel 源（包 p4cgen）——逐字节 diff 门禁目录
generated/p4c_signature/  签名/调度 JSON
project/p4c.sbt        meta-build 钩子（把编译器源码挂进 sbt meta-build）
build.sbt              p4Generate / p4GenerateStaged / p4Stages / p4Clock / p4DelayModel
```

## 3. 快速上手

1. 在 `p4/demos/` 新建 `mydemo.p4`（子集语法见 §4）；
2. `sbt compile`——生成 `generated/p4c/Mydemo.scala`（模块名 = 文件名 PascalCase + control/parser 名）；
3. `sbt test` 或新建 Spec 用 chiseltest 例化 `p4cgen.MydemoIngress` 驱动；
4. 需要 Verilog：把生成的 Module 接入既有 FIRRTL 流程（同仓库其他 Chisel 模块一致）。

CLI 单文件编译（不经 sbt 生成管线）：

```bash
sbt "runMain P4C.P4cMain p4/demos/demo2-match.p4 /tmp/p4c-out"
# 可选旗标见 §6
```

## 4. P4 子集参考

```p4
#include <core.p4>                          // 仅按声明跳过，无语义

header ethernet_h {                          // header 类型：bit<N> 字段
    bit<48> dstAddr;
    bit<16> etherType;
}
struct headers_t   { ethernet_h ethernet; }  // struct：header 实例 + bit<N> 成员
struct metadata_t  { bit<16> normPort; bit<8> cls; }

control Ingress(inout headers_t hdr, inout metadata_t meta) {
    action set_cls(bit<8> c) {               // action：形参 bit<N>，体 = 赋值 / extern 调用
        meta.cls = c;
        meta.normPort = (bit<16>)(hdr.ethernet.srcAddr[15:0] + 1) << 1;  // 表达式子集
    }
    Register(bit<16>, 8) stats;              // extern：Register(bit<W>, N) / Counter(bit<W>, N)
    Counter(bit<32>, 8) hits;

    table cls_table {                        // 静态表：const entries 编译期融合
        key = { hdr.ethernet.etherType : exact; }   // key 须为字段路径，仅 exact
        actions = { set_cls; nop; }
        const entries = {
            0x0800 : set_cls(8w7);           // key 常量 : action(实参)
            default : nop();
        }
    }

    // p4c: table rt_table runtime size=6    ← 运行时表编译指示（紧邻 table 行，size 缺省 4）
    table rt_table {
        key = { hdr.ethernet.etherType : exact; }
        actions = { set_cls; set_port; nop; }
        const entries = { default : nop(); } // 运行时表仅允许 default 行（可省略）
    }

    apply {                                  // 顺序块
        cls_table.apply();
        rt_table.apply();
        stats.write(4w0, stats.read(4w0) + 16w1);  // Register read/write
        hits.count(4w0);                     // Counter count
    }
}

parser Top(packet_in pkt, out headers_t hdr) {
    state start {
        pkt.extract(hdr.ethernet);           // 每状态提取固定偏移的 header
        transition select(hdr.ethernet.etherType) {
            0x0800 : parse_ipv4;
            default : accept;
        }
    }
    state parse_ipv4 {
        pkt.extract(hdr.ipv4);
        transition accept;
    }
}
```

表达式子集：`+ - & | ^ << >> ++`（拼接）、位切片 `x[hi:lo]`、比较 `== != < <= > >=`、
`&& ||`、三元 `? :`、`~ !`、`(bit<N>)` 转换、带宽字面量 `16w0x0800`。
宽度规则：拼接 = 两操作数宽之和（左高）；移位取左操作数宽；双目取 max（显式 Zext/Trunc）；
赋值显式 fit 到目标宽。**绝不静默截断**，越界字面量报 P4Error。

不支持：`* / %`、`if`/`switch`、lpm/ternary、meters/checksum extern、`if hit`。

## 5. 编译指示（`// p4c: ...`）

- 形态：`// p4c: stages=N`、`// p4c: table <表名> runtime [size=N]`（大小写/空格不敏感，行尾尾巴忽略）。
- **紧邻性**：指示必须紧邻其作用的声明行（之间只允许空行；隔代码/注释行 → 忽略 + 告警）。
- `table` 指示的表名是冗余校验：与声明名不一致 → P4Error（含两个表名）。
- 块注释内的指示样文本不生效（告警提示）。
- 优先级：声明级 `stages=N` > clock 模式 > 全局 `--stages`。

## 6. 编译入口与参数

### 6.1 CLI（`P4C.P4cMain`）

```
P4cMain <in.p4> <outDir> [copyDir] [--stages N] [--clock W] [--sig-dir <dir>] [--delay-model weighted|unit|<file.json>]
```

| 旗标 | 说明 |
|------|------|
| `--stages N` | 全局切拍预算（N ≥ 1；1 = 不切拍，与历史输出逐字节一致） |
| `--clock W` | 每级最大组合延迟上限（无量纲权重），逐 DAG 自动搜最小可行级数；低于单节点最大权重时报 P4Error 附最小可行周期；**与 `--stages` 互斥** |
| `--sig-dir <dir>` | 输出签名/调度 JSON（`<前缀>.json`，格式见 §8） |
| `--delay-model` | 延迟模型：`weighted`（默认）/ `unit` / `logiceffort`（Logic Effort，ND2 归一化）/ 外部 JSON 路径（格式见 §9） |

### 6.2 sbt 集成（build.sbt）

| setting | 环境变量 | 默认 | 说明 |
|---------|---------|------|------|
| `p4Stages` | `P4C_STAGES` | 1 | 主 demo 管线全局切拍预算 |
| `p4Clock` | `P4C_CLOCK` | 0（关闭） | clock 模式（>0 启用） |
| `p4DelayModel` | `P4C_DELAY_MODEL` | weighted | 延迟模型 |
| `p4GenerateStaged` | `P4C_STAGED_STAGES` | 4 | staged/ 目录变体拍数 |

例：`P4C_CLOCK=2 sbt compile` 按 clock 模式重生成全部 demo。

## 7. 运行时表使用

### 7.1 写接口（每表 3 个端口）

```
io.tbl_<表名>_we     : Input(Bool())          写使能
io.tbl_<表名>_waddr  : Input(UInt(addrW.W))   地址（addrW = max(1, log2ceil(size))）
io.tbl_<表名>_wdata  : Input(UInt(entryW.W))  整条目（单字原子提交）
```

- 时钟沿提交，`we=1 && waddr < size`；越界写被忽略（表内容不变）；
- **删除**：写 valid=0 的条目；
- **可见性**：写在时钟沿提交、查找是寄存器值的组合函数——写拍当拍的查找看到旧值，下一拍起看到新值（绝不撕裂）；写口与查找无互锁；
- **上电**：全 0 ⇒ 空表全 miss，走编译期固定的 default action。

### 7.2 条目编码（MSB → LSB，生成文件头注释有逐表回显）

```
[entryW-1]                valid（1 = 有效）
[entryW-2 : argW+keyBits] actionId（按 actions 声明序 0..k-1；actW = max(1, bits(k-1))）
[argW+keyBits-1 : keyBits] 参数位串（argW = max(各 action 参数总宽)；多参数按声明序
                            先声明在高位，未用高位补 0）
[keyBits-1 : 0]           key（多 key 时 Cat 拼接，先声明在高位）
```

例（demo7：size=6, keyBits=16, actW=2, argW=24, entryW=43）：

```scala
// 写 addr=0：key=0x0800 → set_port(p=0x4321, t=0x56)（actId=1）
val wdata = (BigInt(1) << 42) | (BigInt(1) << 40) | (BigInt(0x4321) << 24) | (BigInt(0x56) << 16) | 0x0800
c.io.tbl_rt_table_we.poke(true.B); c.io.tbl_rt_table_waddr.poke(0.U)
c.io.tbl_rt_table_wdata.poke(wdata.U(43.W)); c.clock.step(1)
c.io.tbl_rt_table_we.poke(false.B)
```

- 多命中语义：**低地址优先**（与静态表声明序同构）；
- 写入未定义 actionId → 所有选通为假，等同 default；
- Top 组装时写口透出为 `io.tbl_<表名>_*` 并直连 control；
- **限制**：运行时表不支持切拍（`--stages>1` / `--clock` 下 P4Error）；default 不支持运行时改写。

## 8. 签名/调度 JSON（`--sig-dir` / sbt 自动输出）

```json
{ "source": "demo7-runtime-table.p4",
  "controls": [ {
    "module": "Demo7RuntimeTableIngress",
    "ports":  [ {"path":"hdrIn.ethernet.etherType","dir":"input","width":16}, ... ],
    "tables": [ {"name":"rt_table","runtime":true,"size":6,"keyBits":16,
                 "actW":2,"argW":24,"entryW":43,"addrW":2}, ... ],
    "externs": [ {"name":"stats","kind":"Register","width":16,"size":8} ],
    "dags":    [ {"ctx":"control Ingress/action set_cls","stageCount":2,
                  "nodes":[{"id":0,"op":"InputRef","width":8,"stage":0}, ...]} ]
  } ] }
```

- 端口按 Bundle 展平为点分路径叶子（`hdrIn.ethernet.etherType`）；向量端口带 `vecSize`；
- `dags[].nodes` = 节点 → 流水级映射（未调度全 0 级）——上位机按端口编程、回归工具按调度分析；
- 供上位机/固件按已知地址空间编程（表深/key 宽/actW/argW 编译期固定并回显）。

## 9. 延迟模型与 clock 预算口径

**所有权重以 ND2（二输入 NAND）门延迟为单位，ND2 一级 = 1.0**（Logic Effort 口径）。`--clock W` 的语义即"每拍最多容纳 W 个 ND2 级"。

### 9.1 内置 `logiceffort` 模型（推荐配合 clock 使用）

| op | ND2 倍数 | 依据 |
|----|---------|------|
| Const/InputRef/Cat/Slice/Zext/Trunc | 0 | 纯布线 |
| Not（INV） | 0.6 | g·h+p = 2τ，ND2 = 10/3 τ |
| And/Or（NAND/NOR+INV） | 1.6 | 两级门 |
| Mux（2:1） | 1.2 | g=2, p=2 |
| Xor（XOR2） | 3.0 | g=4, p=6 |
| Add/Sub（w 位） | w | 行波进位链上界（综合可建 CLA 更快，高估保守） |
| Shl/Shr（w 位） | 1.2·log2(w) | 桶形移位：log2(w) 级 2:1 mux |
| Eq/Neq（w 位） | 3.0+1.6·log2(w) | 按位 XNOR + AND 归约树 |
| Lt/Le/Gt/Ge（w 位） | 3.0+2.4·log2(w) | 树形比较器 |
| RegRead（size 项） | 1.2·log2(size) | 读 mux 树 |

示例：`P4C_DELAY_MODEL=logiceffort P4C_CLOCK=24 sbt compile`——每拍 ≤ 24 个 ND2 级；16 位加法器占 16 级，clock=8 时会如实报"不可行（最小可行 clock = 16）"（节点原子、不可跨级切分）。

### 9.2 外部延迟模型 JSON

```json
{ "Const":0, "InputRef":0, "Cat":0, "Slice":0, "Zext":0, "Trunc":0, "Not":0.6,
  "Bin":1.6, "Bin(Add)":16, "Mux":1.2, "RegRead":3.6 }
```

- 必需项：Const/InputRef/Cat/Slice/Zext/Trunc/Not/Bin/Mux/RegRead（缺项 P4Error）；**允许小数**（ND2 倍数口径）；
- `Bin(Add)` 细分覆盖 `Bin` 通配；
- 数值口径与 `--clock` 一致：均为 ND2 级数。

## 10. 生成代码与集成

- 模块命名：`<文件名Pascal><Control 名>`（如 `Demo2MatchIngress`）、parser `<前缀><Parser 名>Parser`、管线 Top `<前缀>Top`；Bundle 每文件自带带前缀定义（多文件无类名冲突）。
- control io：`<param>In/<param>Out`（Bundle 展平）、`valid`（有 extern 或切拍时）、`ex_<inst>` 观测口、`outValid`（切拍/clock 模式）、运行时表 `tbl_*`。
- **切拍时序契约**：`io.valid` 单拍脉冲、发起间隔 ≥ N（Top 一次性 fire 天然满足；独立例化 control 的下游自行保证）；输入字段在调用期间保持稳定。
- 生成文件头部含：切拍契约（N>1 时）/ 运行时表协议注释（有运行时表时）——先读头注释再用模块。

## 11. 测试与门禁

| 套件 | 内容 |
|------|------|
| `CrossEngineFuzzSpec` | 交叉引擎 fuzzer：IR 解释器（黄金）vs 生成 RTL，固定 seed 随机比对（demo1/demo2/demo7） |
| `Demo1~8*Spec` | 各 demo 端到端 chiseltest（含运行时表八条矩阵） |
| `StagedEquivalenceExtraSpec` 等 | 切拍 N=1 vs N=3/4 行为等价 |
| `SchedulerSpec` / `DelayModelSpec` / `DirectiveSpec` / `IrPassSpec` / `SignatureSpec` | 调度/模型/指示/优化 pass/签名单测 |
| 基线门禁 | `sbt clean compile` 后 `diff -r generated/p4c /tmp/p4c-baseline-x5` 逐字节一致（当前基线 12 文件；生成行为变化时按流程重录） |

## 12. 已知限制与 FAQ

- **运行时表 + 切拍**：报 P4Error（本期仅 N=1）；需切拍时把运行时表换成静态表或等后续立项。
- **lpm/ternary**：不支持；建议多条 exact + 软件侧保证唯一前缀折中（见增量 PRD P1-1）。
- **SRAM/TCAM 映射**：未接；运行时表当前 Vec[Reg]（组合读、查找零拍）。
- **`if hit` / `switch`**：不支持；用多表串接（apply 顺序块）表达。
- **macOS 大小写不敏感 FS**：生成包名固定 `p4cgen`（`p4c.gen` 会与 `P4C` 目录冲突）。
- **Chisel 3.6 注意**：switch 无 default（生成器已用 when/elsewhen/otherwise）；Bundle 勿手写 cloneType；切拍 valid 链必须 RegNext 纯延迟线。
- **改了 `.p4` 没生效**：sourceGenerator 以内容 hash 触发，`sbt clean compile` 强制重生成。
