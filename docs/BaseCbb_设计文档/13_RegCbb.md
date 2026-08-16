# RegCbb — 寄存器文件框架设计文档

> 路径：`src/main/scala/BaseCbb/RegCbb/`　包：`BaseCbb.RegCbb`（v2 结构，原 v1 已删除）
> 本文档描述 v2 的完整设计：单一事实源 IR → 地址分配 → RTL 生成 → 用户连接视图 → 文档/软件视图生成。

---

## 1. 设计目标与总体架构

RegCbb_v2 以 **单一事实源（Single Source of Truth）** 为核心理念：寄存器/存储器的定义只写一次（IR），
其余所有产物——RTL、用户连接接口、地址映射、C 头文件、JSON、Markdown/HTML 手册——全部从 IR 推导生成，
杜绝"定义漂移"（RTL 与文档不一致）。

```
                    ┌──────────────────────────────────────────┐
                    │  IR（中间表示）Def.scala                   │
                    │  RegBlockDef / RegDef / RegFieldDef /     │
                    │  MemoryDef                                │
                    └───────────────┬──────────────────────────┘
                                    │
                     ┌──────────────┴──────────────┐
                     ▼                             ▼
        ┌────────────────────┐        ┌──────────────────────┐
        │ AddressAllocator   │        │  DSL 入口             │
        │ 字段位/寄存器字节/  │        │  RegBlockDsl.scala    │
        │ 存储器基址 分配     │        │  RegBundle.scala      │
        └────────┬───────────┘        └──────────────────────┘
                 ▼ (RegFileMap)
   ┌─────────────┼──────────────────────────┐
   ▼             ▼                          ▼
hw/RegCore.scala  hw/RegView.scala        gen/ (JsonGen, CHeaderGen,
hw/AxiLite.scala  （用户连接视图）          MarkdownGen, HtmlGen,
  RTL 生成                                  ViewSourceGen) 文档/软件视图
```

### 关键文件

| 文件 | 职责 |
|------|------|
| `Def.scala` | IR 定义：`AccessType`/`HwAction`/`WriteAction`/`MemoryAccessType` 枚举、`RegFieldDef`/`RegDef`/`MemoryDef`/`RegBlockDef` |
| `AddressAllocator.scala` | 地址分配：字段 LSB-first 紧凑排布、寄存器 32bit word 对齐、存储器自然对齐自动分配 |
| `dsl/RegBlockDsl.scala` | 字段级 DSL：`RegField`/`RegBuilder`/`MemBuilder`/`BlockBuilder`/`RegBlock` |
| `dsl/RegBundle.scala` | GenBundle（Bundle 式）风格定义 + `BundleToRegDefs` 转换器 |
| `hw/RegCore.scala` | RTL 核心：`DecIO`/`FieldReg`（单寄存器字段级语义）/`RegFileTop`（总线解码 + memory 访问）/`MemPortIO`（请求-响应协议）/`MemStatus` |
| `hw/RegView.scala` | 用户逻辑连接视图：`RegView`/`RegHandle`/`FieldHandle`（命名访问 + 自动位域切割） |
| `hw/AxiLite.scala` | AXI4-Lite 包装器：`AxiLiteBusIO`/`AxiLiteRegFile` |
| `gen/*.scala` | 5 个生成器：JSON / C 头 / Markdown / HTML / 具名视图源码 |
| `demo/UartDemo.scala` | 完整 Demo（UART 外设），含 `EmitAll` 一键生成 |

---

## 2. IR 定义（Def.scala）

### 2.1 访问类型 `AccessType`

| 类型 | 软件可读 | 软件可写 | 读副作用 | 硬件动作(hwAction) | 语义 |
|------|:---:|:---:|:---:|---|---|
| `RW`  | ✓ | ✓ | - | - | 读写 |
| `RO`  | ✓ | ✗ | - | - | 只读（硬件驱动） |
| `WO`  | ✗ | ✓ | - | - | 只写（读回 0） |
| `RC`  | ✓ | ✓ | ✓ | Set | 读后清零 |
| `RS`  | ✓ | ✓ | ✓ | Clear | 读后全置 1 |
| `W1C` | ✓ | ✓ | - | Set | 写 1 清零 |
| `W1S` | ✓ | ✓ | - | Clear | 写 1 置位 |
| `W1T` | ✓ | ✓ | - | Toggle | 写 1 翻转 |

- `hasReadEffect`：RC/RS 读操作会修改存储。
- `hwAction`：W1C/RC 可由硬件**置位**、W1S/RS 可由硬件**清除**、W1T 可由硬件**翻转**。
- `WriteAction`（Normal/OneToClear/OneToSet/OneToToggle/ClearOnRead）仅供文档描述用，硬件语义由 AccessType 决定。

### 2.2 `RegFieldDef` — 字段定义

```scala
case class RegFieldDef(name, bitWidth, access = RW, resetValue = 0,
                       description = "", writeAction = Normal,
                       enumerations: Map[BigInt, (String, String)] = Map.empty)
```
- 校验：`1 ≤ bitWidth ≤ 256`；`resetValue ≥ 0` 且 ≤64bit 时须小于 `2^bitWidth`。
- `enumerations`：枚举值 →（名称, 描述），用于文档与 JSON。

### 2.3 `RegDef` — 寄存器定义

```scala
case class RegDef(name, fields: Seq[RegFieldDef], description = "", group = None, atomic = true)
```
- 校验：字段非空、字段名不重复。
- 派生：`totalBits`（字段位宽和）、`wordCount = ceil(totalBits/32)`、`byteSize = wordCount*4`。
- `atomic`：**多字（>32bit）寄存器**的写模式：
  - `atomic=true`（默认）：写低字进 shadow 暂存，写**最高字**时一次提交完整值 → 软件永远读不到中间态；
  - `atomic=false`：逐字直接写对应位域 → 中间态可被软件读观测（适合不关心一致性的场景）。

### 2.4 `MemoryDef` — 存储器定义

```scala
case class MemoryDef(name, depth, dataWidth, memType = SP, baseAddress = None,
                     description = "", atomic = true)
```
- 校验：`depth > 0`；`dataWidth` 为 32 的整数倍且 ∈ [32, 256]；`baseAddress ≥ 0`。
- 派生：`addrWidth = ceil(log2(depth))`（按 dataWidth 单元编址）、`wordCount = dataWidth/32`、`byteSize`。
- `baseAddress=None` 时由 `AddressAllocator` 自动分配（从块 memBaseAddress 起按 dataWidth/8 自然对齐）。
- `atomic`：总线（32bit）访问宽存储时的写模式，语义同寄存器原子模式。

### 2.5 `RegBlockDef` — 寄存器块

```scala
case class RegBlockDef(name, regBaseAddress, memBaseAddress, registers, memories = Seq.empty,
                       description = "", deviceName = "")
```
- `devName`：文档/头文件使用的设备名（缺省 = name）。

---

## 3. 地址分配（AddressAllocator.scala）

`AddressAllocator.allocate(block: RegBlockDef): RegFileMap` 一次完成三类分配：

1. **字段位偏移**：同寄存器内 LSB-first 紧凑排列（`FieldAllocation(field, bitOffset)`）。
2. **寄存器字节偏移**：从 0 起按 `byteSize` 递增（`RegAllocation(reg, byteOffset, fieldAllocations)`）。
   每个寄存器按 32bit word 对齐，**寄存器绝不跨越 word 边界**，保证多字寄存器的 word 选择逻辑简单。
3. **存储器基址**：从块 `memBaseAddress` 起按 `dataWidth/8` 字节自然对齐依次分配；
   手工指定 `baseAddress` 时自动跳过已占用区间（避免重叠）。

输出 `RegFileMap(block, regs, mems)` 是 RTL / 文档 / 软件视图共用的完整映射；
`summarize` 输出人类可读的分配摘要（调试用）。

---

## 4. RTL 生成（hw/RegCore.scala）

### 4.1 总线侧接口 `DecIO`

```scala
class DecIO(dataWidth) { wr, wdata, rd, rdata }   // 单拍读写
```
`RegFileTop` 以简单总线（wr/rd/addr/wdata/rdata，字节地址，32bit）对外；
`AxiLiteRegFile` 将 AXI4-Lite 转换为该内部总线。

### 4.2 单寄存器 `FieldReg`

每个寄存器实例化一个 `FieldReg(alloc, dataWidth)`，内部完成：

- **存储**：非 RO 字段各一个 `RegInit(resetValue)`（按字段宽度）。
- **读组装**：`readVal = fold(fields)` 把各字段值（RO 取 `roValue` 端口、WO 恒 0、其余取存储）
  按位偏移贴合成全宽值；当前 word 读出 `rdata = readVal >> (wordSel*dataWidth)`。
- **写语义**（`applyFieldSemantics`）：
  - RW/WO/RC/RS：直接覆盖；
  - W1C：`st & ~newBits`；W1S：`st | newBits`；W1T：`st ^ newBits`。
- **多字原子模式**：`shadow_w0..shadow_w(last-1)` 暂存低字，写最高字时
  `commitVal = {wdata[高字], shadow…}` 一次性提交到存储；
- **多字非原子模式**：`applyWordWrite` 只更新与所写 word 相交的字段位（含跨 word 字段的部分位更新）。
- **HW 写入路径**（按字段类型）：`hwSet`(W1C/RC 置位)、`hwClr`(W1S/RS 清除)、`hwTog`(W1T 翻转)、
  `hwWrEn/hwWrData`(RW 直写)。
- **读副作用**：`when(rd)` 时 RC 清零、RS 全置 1。
- **冲突优先级**：SW 写 > 读副作用(RC/RS) > HW set/clr/tog。
- **用户侧输出**：`wrEn/wrData`、`rdEn/rdData`（各延迟 1 拍捕获）、`value`（组合当前值）。

### 4.3 用户连接面 `RegCoreIO`

按字段名组织的 Record 型端口集合（方向为"从寄存器文件向外看"）：

| 端口 | 类型 | 说明 |
|------|------|------|
| `wrEn/wrData` | Output | SW 写脉冲 + 全宽数据（同拍） |
| `rdEn/rdData` | Output | SW 读脉冲 + 读回数据 |
| `value` | Output | 当前值（组合，含 RO 位） |
| `roValue.*` | Input | RO 字段驱动（按字段名） |
| `hwSet.*` | Input | W1C/RC 字段置位 |
| `hwClr.*` | Input | W1S/RS 字段清除 |
| `hwTog.*` | Input | W1T 字段翻转 |
| `hwWrData.*` / `hwWrEn` | Input | RW 硬件直写 |

### 4.4 顶层 `RegFileTop`

```scala
class RegFileTop(map, addrWidth = 32, dataWidth = 32)
```
- 寄存器解码：`regHits` 按 `regBaseAddress + byteOffset` 匹配；多字寄存器命中地址范围 `[base, base+byteSize)`，
  `wordSel = (addr - base) >> 2` 的低 `log2Ceil(wordCount)` 位。
- 存储器访问：每片 memory 一个 **4 态访问状态机**（见 4.5）。
- `io.rdata = MuxCase(0, regHits→rdata, memHits→memRdata)`。

### 4.5 存储器请求-响应协议 `MemPortIO` + `MemStatus`

```scala
class MemPortIO(addrWidth, dataWidth) {
  rd / raddr / wr / waddr / wdata   // Output：请求（电平）
  rdata / ack / status              // Input：响应
}
```

**协议**（用户侧外部 SRAM 包装实现）：
- **读**：`rd` 拉高（**ack 返回前一直保持高电平**），`raddr` 有效；
- **写**：`wr` 拉高（**ack 返回前一直保持高电平**），`waddr`/`wdata` 有效；
- **响应**：完成的那一拍**同拍**置 `ack`，读数据 `rdata` 与 `status[2:0]` 同时有效；
- `status` 编码（`MemStatus`）：

| 编码 | 名称 | 含义 |
|------|------|------|
| `000` | `OK` | 读数据 OK / 写完成 |
| `001` | `TIMEOUT` | 超时 |
| `010` | `UNCORRECTABLE` | 读数据不可纠正错误 |
| 其余 | - | 保留 |

- 未挂接时输入默认 0（`ack=0` 表示永远不响应）。

**访问状态机**（每片 memory，4 态）：

| 状态 | 说明 |
|------|------|
| `stIdle` | 接受请求：写→`stWrWait`（或低字写 shadow / 非原子读-改-写）；读→`stRdWait` |
| `stRdWait` | `rd` 保持拉高，等 `ack` |
| `stWrWait` | `wr` 保持拉高，等 `ack` |
| `stRmwRead` | 非原子多字写：先 `rd` 读整字 → `ack` 后合并 `MuxLookup(memWord, …)` 贴新字 → `stWrWait` 写回 |

**宽存储（dataWidth > 32）总线访问**：
- 原子模式：内部 `Mem(depth*wordCount, UInt(32.W))` shadow 暂存低字，写最高字时 `commitWdata` 组装完整值一次写；
- 非原子模式：读-改-写流程。
- 读响应：`status==OK` 时取 `rdata` 的对应 word，否则返回 0；`ack` 拍锁存供总线后续读出。

### 4.6 AXI4-Lite 包装器 `AxiLiteRegFile`

- 单笔在途写事务（`aw_ready/w_ready/ar_ready` 恒真，不流水）；
- 写响应 `b_valid` 在 `wHand` 下一拍置起，`b_hand` 后清除；
- 读：`arHand` 时内层组合 `rdata` 锁存，`r_valid` 下一拍置起（1 拍读延迟）；
- `io.user`/`io.memPorts` 与内层 `RegFileTop` 透传（`CoreConnect`/`MemConnect`）。

---

## 5. 用户连接视图（hw/RegView.scala）

`RegView(map, user)` 提供命名访问，隐藏 Record 细节：

```scala
regs("ctrl").field("baud_div").value     // 当前值（位域自动切割）
regs("ctrl").field("tx_en").wrEn          // SW 写脉冲
regs("tx_data_wo").field("data").wrData   // 写数据（同拍）
regs("status_ro").field("tx_busy").roValue := txBusy   // RO 驱动
regs("irq_w1c").field("tx_done").hwSet := txDonePulse   // W1C 硬件置位
```

- `FieldHandle`：`wrEn/rdEn/wrData/rdData/value`（寄存器级信号按字段位域切割）+ `roValue/hwSet/hwClr/hwTog/hwWrData`（字段级真实端口）。
- `RegHandle`：寄存器级信号 + `fields/field(name)/apply(name)`。
- `RegView`：`reg(name)`/`apply(name)`/`names`/`all`/`writableRegs`/`roRegs`/`readNotifyRegs`；
  构造时把所有用户侧输入端口默认置 0/false（**不能用 DontCare**——hwWrEn/hwSet 等会被硬件消费，
  x 值在仿真中会随机改写寄存器内容），用户后续显式连接覆盖默认值。
- 三个工厂：`apply(map, user)` / `apply(map, RegFileTop)`（存储器响应默认 0）/ `apply(map, AxiLiteRegFile)`。

---

## 6. DSL 入口

### 6.1 字段级 DSL（dsl/RegBlockDsl.scala）— 推荐入口

```scala
RegBlock("uart") { b =>
  b.device("UART").baseAddress(0x40000000L).memBaseAddress(0x40001000L)
  b.reg("ctrl") { r =>
    r.field(RegField.rw("tx_en", 1, 0, "发送使能"))
    r.field(RegField.ro("version", 4, 0, "版本号"))
    r.field(RegField.w1c("tx_done", 1, "发送完成中断"))
  }
  b.mem("tx_fifo") { m => m.depth(64).dataWidth(64).sp().atomic() }
}
```

- `RegField` 便捷入口：`rw/ro/wo/rc/rs/w1c/w1s/w1t`（均支持 `(name, width)`、`(name, width, desc)`、带 block 参数三种形态）。
- `FieldBuilder` 全功能入口：`RegField("x", 8){ f => f.rw().reset(1).desc("…").enum(0,"A","描述").oneToClear() }`。
- `RegBuilder`：`field/desc/group/atomic/nonAtomic`；`MemBuilder`：`depth/dataWidth/sp/tp/baseAddress/atomic`；
  `BlockBuilder`：`device/baseAddress/memBaseAddress/reg/mem/regs(Seq)`。

### 6.2 GenBundle 风格（dsl/RegBundle.scala）

```scala
class UartBundleRegs extends RegBundle {
  val bundle_ctrl = new RegBundle {           // 嵌套 = 一个寄存器（字段紧凑 LSB-first）
    val mode  = UInt(2.W)
    val burst = Bool()
    Attr += (mode  -> FieldAttr("工作模式", reset = 1))
  }
  val bundle_status_ro = new RegBundle { … }   // 命名后缀 _ro → 整寄存器 RO
  val bundle_scratch_ro = UInt(8.W)            // 叶子元素 = 单字段寄存器
}
b.regs(BundleToRegDefs.toRegDefs(new UartBundleRegs))
```

- `FieldAttr(desc, reset, access)` 注解：显式 `access` 优先于命名后缀推断。
- 后缀推断表：`_w1c/_w1s/_w1t/_ro/_wo/_rc/_rs`，否则 RW。
- **注意**：Chisel 反射返回 elements 为声明逆序，`BundleToRegDefs` 内部 `.toSeq.reverse` 保证定义顺序。
- `BundleToRegDefs.toBlock(name, bundle, …)` 可直接由 Bundle 构造整块（无 memory）。

---

## 7. 生成器（gen/）

所有生成器输入都是 `RegFileMap`，无外部依赖（手写序列化）：

| 生成器 | 输出 | 内容 |
|--------|------|------|
| `JsonGen` | `*.json` | 完整 IR：设备名/基址/寄存器（含字段位偏移、访问类型、复位、枚举）/存储器 |
| `CHeaderGen` | `*.h` | `REG_BASE/MEM_BASE`、每寄存器地址宏、每字段 `_MASK/_SHIFT/_RST`、存储器宏 |
| `MarkdownGen` | `*.md` | 地址映射表 + 每寄存器位域图（文本）+ 字段表（含枚举）+ 存储器表 |
| `HtmlGen` | `*.html` | 同上，位域图为 div 按位宽比例排列（自包含 CSS） |
| `ViewSourceGen` | `*.scala` | 具名视图类：`class UartRegs(view: RegView){ val ctrl = view("ctrl") … }` → 编译期具名访问 |

位域图细节：`MarkdownGen.bitDiagram` 超宽（>120 字符或 >96bit）时退化为位列表；
`HtmlGen.bitfield` 按字段位宽百分比排列 div。

---

## 8. Demo（demo/UartDemo.scala）

`UartDemoDef` 定义简化 UART 外设，覆盖全部典型场景：

| 寄存器/存储 | 演示点 |
|---|---|
| `ctrl` | 字段级 RW/RO 混排 |
| `status_ro` / `rx_data_ro` | RO 寄存器（用户驱动读回） |
| `tx_data_wo` | WO（写脉冲捕获） |
| `irq_w1c` / `irq_en` | W1C 中断 + 使能 |
| `scratch` | 32bit RW 冒烟测试（复位值 0xDEADBEEF） |
| `data64` | **64bit 原子**寄存器 |
| `data64_plain` | **64bit 非原子**寄存器 |
| `bundle_*` | RegBundle 风格寄存器组 |
| `tx_fifo` / `tx_fifo_plain` | **64bit 宽 memory 地址空间**（原子 / 非原子） |

`EmitAll` 一键生成到 `generated/RegCbb_v2/`：`uart_regs.json / .h / .md / .html / UartRegs.scala / UartDemo.sv / UartAxiDemo.sv`。

---

## 9. 与 v1 的差异与设计注意

- **v1 已删除**（`RegType/dec_if/rc_core_if/RegFieldDsl/RegisterIRGenerator` 等），v2 自包含。
- v1 的 RO 寄存器"用户驱动读回恒 0"bug 在 v2 修复（`RegCoreIO.roValue` 独立输入端口）。
- **不变量**：字段不重叠（`FieldReg` require）；寄存器不跨 word 边界（`AddressAllocator` 保证）；
  存储器地址不与寄存器/其它存储器重叠（自动分配跳过已占用区）。
- **已知限制**：`RegFileTop` 仅支持 32bit 总线（`require(dataWidth == 32)`）；
  AXI-Lite 为单笔在途事务（无流水）；原子写需要软件按"低字→高字"顺序写。
- 测试：`src/test/scala/BaseCbb/RegCbb/UartRegTest.scala`（243 行，覆盖原子/非原子/宽存储/中断语义）。
