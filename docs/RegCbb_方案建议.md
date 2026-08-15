# RegCbb 寄存器定义与外围逻辑连接 —— 实现方案建议

> 阅读对象：`src/main/scala/BaseCbb/RegCbb` 及其 `dsl/` 子目录
> 本文先盘点现状与关键问题，再给出"定义寄存器 + 连接外围逻辑 + 自动生成代码/文档"的推荐实现方案，最后给出分步落地路线。

---

## 1. 现状盘点

### 1.1 两条并行的定义路径

| | 路径 A：声明式字段 DSL | 路径 B：GenBundle 风格 |
|---|---|---|
| 定义文件 | `dsl/RegisterBlockDsl.scala`、`dsl/RegFieldDsl.scala`、`dsl/MemoryDef.scala` | `dsl/GenRegBlock.scala`（+ 基类 `BaseCbb.GenBundle`/`fldAttr`，原在 `utils/GeneratorLib.scala`） |
| 定义方式 | `RegBlock("dev"){ b.reg("ctrl"){ r.field(RegField.rw("enable",1,0,"desc")) } }` | `class If extends GenBundle { val ctrl = new GenBundle{...}; val status_ro = ... }` |
| 地址分配 | `dsl/AddressAllocator.scala`（自动字节/字对齐） | 隐式：`regBaseAddr + i`，每寄存器一个地址槽 |
| RTL 生成 | `dsl/RegisterFileGenerator.scala`（单模块扁平化） | `GenRegBlock.RegBlock / AxiLiteRegBlock / AxiRegBlock`（每寄存器一个类型模块） |
| 连接层 | 无（`regDataOut` 是索引 Vec） | `dsl/RegConnect.scala`（按名访问 + 自动位域切割） |
| 文档/IR | `dsl/RegisterIRGenerator.scala` → JSON + C 头文件 | 无任何 IR/文档输出 |
| 现状 | 无活跃使用（`src/test/scala/RegCbb/AxiRegBlockSample.scala` 整体被 `/* */` 注释） | 示例与演示均走此路径（`example/GenRegBlockSample`、`RegConnectSample`、`AxiRegBlockBurstSample`） |

### 1.2 寄存器类型与双接口

`RegType.scala`（`dsl/RegType.scala`，包 `BaseCbb.RegCbb`）定义了 8 种寄存器模块：`RW / RO / WO / RC / RS / W1C / W1S / W1T`。

每个模块两个接口：

- **dec（总线侧）**：`dec_if[T]` — `in.wr / in.wdata / in.rd`、`out.rdata`；
- **core（用户逻辑侧）**：`wrEn + wrData`（RW/WO/W1C/W1S/W1T，wrEn 为**一拍写脉冲**）、`wrData` 输入（RO，由用户驱动）、`rdData + rdEn`（RC/RS，读脉冲）。

`GenRegBlock.RegBlock` 把这些模块统一包成：

- `io.wr / io.rd / io.addr / io.wdata / io.rdata` —— 简单总线（或经 `AxiLiteRegBlock`/`AxiRegBlock` 接 AXI）；
- `io.cores.<寄存器名>` —— Record，每个寄存器一个 `RegCoreOutput(wrEn, wrData, rdData, rdEn)`，**位宽=该寄存器 totalBits**；
- `io.roInputs(i)` —— RO 输入 Vec（固定 64bit，按 RO 序号映射）。

### 1.3 自动生成能力现状

- RTL：两套生成器（路径 A 扁平单模块；路径 B 类型化多模块）；
- 软件视图：JSON（`RegisterIRGenerator.toJson`）+ C 头文件（`toCHeader`）；
- **寄存器文档（Markdown/HTML/位域图）生成器不存在**；
- 路径 B 完全不产出任何 IR/文档。

---

## 2. 读代码发现的关键问题

1. **`GenBundle`/`fldAttr` 源码已被删除**。`BaseCbb.utils.GeneratorLib.scala` 在工作区被删除（git 已 staged delete），但 `GenRegBlock.scala`、`RegConnect.scala` 及全部示例都 `import BaseCbb.{GenBundle, fldAttr}`。目前 `target/scala-2.13/classes` 里残留旧 class 所以增量编译侥幸通过，**一旦 clean 构建必然失败**。这是第一优先修复项（从 git 恢复或把定义内联进 `RegCbb`）。

2. **RO 寄存器读回恒为 0（功能性 bug）**。`GenRegBlock.RegBlock` 解码循环里：
   ```scala
   case "RO" => rdataMux(i) := 0.U
   ```
   RO 模块 `ro.io.dec.rdata`（= `core.wrData`，即用户经 `roInputs` 驱动的值）**从未被接入读数据 mux**，软件读 RO 寄存器恒得 0。`RegConnectSample` 只驱动了 `c.roInput("status_ro")` 却未验证读回，故未被发现。修法：`rdataMux(i) := reg.asInstanceOf[BaseCbb.RegCbb.ro].io.dec.rdata`。

3. **AXI 包装器不驱动 RO 输入**。`AxiLiteRegBlock`/`AxiRegBlock` 只暴露 `io.axi + io.regs`，内部 `RegBlock.io.roInputs` 悬空（DontCare），且 `io.regs := regBlock.io.cores` 里 RO 的 `rdData` 恒 0 —— AXI 场景下 RO 寄存器完全不可用；也没有用户侧驱动 RO 的端口。`RegConnect` 也只支持裸 `RegBlock`，不支持 AXI 包装器。

4. **多字（>32bit）寄存器与 32bit 总线不匹配**。`RegBlockIO.wdata` 固定 32bit，`wdata.asTypeOf(UInt(totalBits.W))` 对 64bit 寄存器只写入低 32bit，高 32bit 永远写不进（例子里 `data64` 实际不可用）；地址解码按 `regBaseAddr + i` 每寄存器只占一个地址槽，与路径 A 的 4 字节/多字对齐语义不一致。

5. **访问类型粒度不一致**。路径 B 按**寄存器**粒度推断访问类型（命名后缀 `_ro/_w1c/...`），一个寄存器内不能 RO/RW 字段混排；路径 A（`RegFieldDef.access`）支持**字段级**访问类型但缺连接层。`RegisterFileGenerator` 生成的 RTL 里 RO 字段没有用户驱动输入，只能当复位值只读。

6. **RO 输入接口不友好**。`io.roInputs` 是 `Vec(RO 数, UInt(64.W))` + 序号映射，绕一层 `RegConnect.roInput(name)` 才能用；`RegBlockIO` 对 >64bit 的 RO 也没有支持（截断逻辑只处理 `>=64` 分支）。

7. **文档生成只有 JSON + C header**。`RegisterIRGenerator` 产出了非常完整的信息（字段名/偏移/宽度/访问/复位/枚举/描述/分组），但**缺 Markdown/HTML 寄存器手册、位域图、SystemRDL/IP-XACT 等标准交换格式**；且该生成器只服务于路径 A，路径 B 无任何输出。

8. **`RegisterFileModule.memPortIO` 与 `MemoryDef.addrWidth` 不一致**：直连内存端口固定 10bit 地址、32bit 数据，而 `MemoryDef` 可配 `addrWidth/dataWidth`（如 1024×64 的 TP SRAM）；`GenRegBlock` 一侧 `RegBlockIO.memPorts` 也只取**第一个** memory 的宽度参数（`head.addrWidth`），多 memory 宽度不同即错。

9. **连接层是"字符串键"风格**：`c("reg")("field")`，字段名拼错运行时才抛异常，无 IDE 补全/编译期检查；寄存器数量多时易错。

10. **测试被整体注释**（`src/test/scala/RegCbb/AxiRegBlockSample.scala` 全文件 `/* */`），路径 A 无回归保护。

---

## 3. 推荐方案：单一事实源 + 三路生成

### 3.1 总体架构

```
                ┌────────────────────────────────────────────┐
                │  定义层（单一事实源，二选一，可互相转换）        │
                │  A. 字段级 DSL：RegBlock("dev"){...}          │  ← 主推（元数据最全）
                │  B. GenBundle 风格：class If extends GenBundle │  ← 快捷入口（自动转 A 语义）
                └───────────────┬────────────────────────────┘
                                ▼
                ┌────────────────────────────────────────────┐
                │  统一 IR：RegBlockDef（已存在，补齐字段）      │
                │  + AddressAllocator（已存在，自动分配地址）    │
                └───────────────┬────────────────────────────┘
        ┌──────────────┬────────┴───────┬───────────────┐
        ▼              ▼                ▼               ▼
  RTL 生成器      连接层生成器        文档生成器        软件视图生成器
  RegFileTop      RegView           RegDocGenerator   RegisterIRGenerator
  （复用 per-type  （类型安全命名       （Markdown/HTML  （JSON/C header，已有）
   寄存器模块）     访问+自动位域）      位域图，新增）
```

原则：**定义一次，RTL、连接视图、文档、C 头文件全部由同一份 IR 生成**，杜绝"硬件定义与文档漂移"。

### 3.2 寄存器定义（推荐写法）

以现有 `RegFieldDsl.scala` 的 `RegFieldDef` 为原子（已含 name/width/access/writeAction/reset/desc/enumerations），补上 `volatile`、寄存器级描述即够用：

```scala
val uart = RegBlock("uart") { b =>
  b.baseAddress(0x4000_0000L)
  b.desc("UART 外设")

  b.reg("ctrl") { r =>
    r.desc("控制寄存器")
    r.group("control")
    r.field(RegField.rw("tx_en",    1, 0,  "发送使能"))
    r.field(RegField.rw("baud_div", 16, 4, "波特率分频"))
    r.field(RegField.ro("loopback", 1, 20, "回环指示（硬件驱动）")) // 字段级 RO
  }

  b.reg("status_ro") { r =>              // 寄存器级 RO，整寄存器由硬件驱动
    r.desc("状态寄存器")
    r.field(RegField.ro("tx_busy", 1, 0, "发送忙"))
    r.field(RegField.ro("rx_rdy",  1, 1, "接收就绪"))
  }

  b.reg("tx_data_wo") { r =>
    r.desc("发送数据（只写）")
    r.field(RegField.wo("data", 8, "待发送字节"))
  }

  b.reg("irq_status_w1c") { r =>
    r.desc("中断状态（写1清）")
    r.field(RegField.w1c("tx_done", 1, 0, "发送完成"))
    r.field(RegField.w1c("rx_over", 1, 1, "接收溢出"))
    // 枚举值
    r.field(RegField.rw("mode", 2, 0) { f =>
      f.desc("工作模式")
      f.enum(0, "POLL", "轮询")
      f.enum(1, "IRQ",  "中断")
    })
  }

  b.memBaseAddress(0x4000_1000L)
  b.mem("fifo") { m => m.depth(64).dataWidth(32).sp().desc("发送 FIFO") }
}

// —— 编译期一次性生成 ——
val map  = AddressAllocator.allocate(uart)   // 地址自动分配（已实现）
val json = RegisterIRGenerator.toJson(RegisterIRGenerator.generate(map))      // 已实现
val chdr = RegisterIRGenerator.toCHeader(RegisterIRGenerator.generate(map))   // 已实现
val md   = RegDocGenerator.toMarkdown(RegisterIRGenerator.generate(map))      // 建议新增
```

GenBundle 风格保留为**快速原型入口**：`GenRegBlock.extractRegDescriptions` 已把"名称后缀 → 访问类型、`fldAttr.ResetValue → 复位值"自动翻译，只需再补一步输出同一 `RegBlockDef` IR（新增一个 `GenRegBlock.toIR(bundle)` 桥），两条路径即可共用全部生成器。

### 3.3 外围逻辑连接（核心诉求）

连接层的目标是：**写寄存器像写一个"事件 + 值"，读寄存器像读一个"命名信号"，全部自动位域切割、编译期可查**。

**写侧（SW → 用户逻辑）三选一语义：**

| 语义 | 适用类型 | 用户逻辑写法 |
|---|---|---|
| 捕获式（事件脉冲） | RW/WO/W1C/W1S/W1T | `when(regs.tx_data_wo.wrEn) { txByte := regs.tx_data_wo.wrData }` |
| 直通持有式 | RW 配置类 | 直接持有 `regs.ctrl.value`（core 直接暴露寄存器值，写回由寄存器模块完成） |
| 组合更新式 | W1C/W1S/W1T | `regs.irq_status.w1c()` —— 内部 `reg := reg & ~wdata` |

**读侧（用户逻辑 → SW）两形态：**

- 寄存器级 RO：`regs.status_ro := Cat(0.U(62.W), rx_rdy, tx_busy)`（命名端口，替代索引 Vec）；
- 字段级 RO：`regs.ctrl.loopback := loopback`。

**推荐实现：类型安全视图（升级 RegConnect，替代字符串键）**

在 IR 基础上用宏注解（项目已开 `-Ymacro-annotations`）或代码生成一个具体类，每个寄存器是具名字段、每个字段自动按 `bitOffset/bitWidth` 切割：

```scala
// 生成器产出（示意，可手写等价物）
class UartRegs(regBlock: RegFileTop) {
  val ctrl         = new RwReg(regBlock, "ctrl")     // 4bit
  val status_ro    = new RoReg(regBlock, "status_ro") // 2bit
  val tx_data_wo   = new WoReg(regBlock, "tx_data_wo")
  val irq_status_w1c = new W1cReg(regBlock, "irq_status_w1c")
}

// 用户逻辑用法 —— 与手写 RTL 直觉一致、IDE 可补全
regs.ctrl.baud_div := 12.U              // 字段级写（仅低 16 位生效）
when(regs.tx_data_wo.wrEn) { txByte := regs.tx_data_wo.wrData }
regs.status_ro.tx_busy := txBusy        // 寄存器级 RO 驱动
regs.ctrl.loopback     := loopback      // 字段级 RO 驱动
when(regs.irq_status_w1c.wrEn) { irqAcc := irqAcc & ~regs.irq_status_w1c.wrData }
```

关键点：`RegConnect.scala` 的 `FieldAccess/RegisterAccess` 已经实现了位域切割与命名查找，**只需把"字符串键查表"改为"生成具名 val"**（每个寄存器一个 val，字段访问仍复用 `FieldAccess` 的切片逻辑），即可获得编译期安全。RO 输入也应由 `roInputs: Vec` 改为 `roInput(name)` 的命名端口（现有 `RegConnect.roInput` 已做，推广到 AXI 包装器即可）。

**总线接入：** `RegBlock` 的 `wr/rd/addr/wdata/rdata` 简单总线可对接任意内部总线；AXI4-Lite / AXI4-full（含 burst）由现有 `AxiLiteRegBlock`/`AxiRegBlock` 提供 —— 修复第 2 章问题 2/3 后即可用于真实 SoC。

### 3.4 自动文档生成（建议新增 RegDocGenerator）

复用 `RegisterIRGenerator` 的 IR（字段/偏移/宽度/访问/复位/枚举/描述/分组/地址），新增两个输出：

1. **Markdown 寄存器手册**：每寄存器一节 —— 偏移、复位值、访问类型、字段表（偏移/宽度/访问/复位/描述/枚举）、简单的 ASCII/文本位域图；
2. **HTML（可选）**：位域图用 div/table 呈现，供内部 wiki 直接引用。

文档头自动带上设备名、基地址、生成时间与"由 RegCbb 自动生成，勿手改"水印。这样一份定义同时喂给 RTL 与文档，天然一致。

### 3.5 软件视图

- C 头文件（已有）：寄存器基址、每字段 `_MASK/_SHIFT/_RST`、memory 段宏；
- 可扩展：Python/rust 头、SystemRDL、UVM reg model（留作后续，IR 已具备全部信息）。

---

## 4. 分步落地路线

| 阶段 | 内容 | 对应问题 |
|---|---|---|
| P0 | 恢复 `GenBundle/fldAttr`（git 恢复 `utils/GeneratorLib.scala` 或迁入 `RegCbb` 内联），保证 clean 构建 | 问题 1 |
| P1 | 修复 RO 读回（`rdataMux` 接 `ro.io.dec.rdata`）；AXI 包装器补 RO 输入端口并驱动 `roInputs`；`RegConnect` 支持 AXI 包装器 | 问题 2、3、6 |
| P2 | 打通两条路径：`GenRegBlock.toIR(bundle)` 桥；`RegFileTop`（IR → per-type 模块 + cores Record + 命名 RO 输入）替代扁平 `RegisterFileModule` | 问题 5、7 |
| P3 | 新增 `RegDocGenerator`（Markdown/HTML 位域图）；补 JSON 里的多字寄存器/字段级 RO 语义 | 问题 4、7 |
| P4 | 连接层类型安全化：由 IR 生成 `XxxRegs` 具名视图（宏注解或代码生成） | 问题 9 |
| P5 | 多字寄存器（>32bit）按 4 字节对齐 + 多 beat 访问；memory 端口宽度与 `MemoryDef` 一致 | 问题 4、8 |
| P6 | 恢复/重写 `src/test`（poke/peek 覆盖 RW/RO/W1C/RC/burst/文档快照） | 问题 10 |

---

## 5. 最小改动即可用的"现在版"（不重构）

若暂不做统一 IR，仅用现有件也能搭出可用方案，需先打 3 个补丁：

1. `GenRegBlock.RegBlock` 解码循环 RO 分支改为：
   ```scala
   case "RO" =>
     rdataMux(i) := reg.asInstanceOf[BaseCbb.RegCbb.ro].io.dec.rdata
   ```
2. `AxiLiteRegBlock`/`AxiRegBlock` 的 IO 增加 `val roInputs = Input(Vec(roCount, UInt(64.W)))` 并 `regBlock.io.roInputs := io.roInputs`（RO 数量由 `extractRegDescriptions` 统计）。
3. `RegConnect` 增加一个接受 AXI 包装器的工厂（内部映射 `io.regs` + `io.roInputs`）。

之后按 `RegConnectSample` 的写法即可获得"命名寄存器 + 自动位域 + RO 驱动 + 批量迭代"，配合 `AddressAllocator.summarize` 打印布局做人工文档，直至 P2/P3 落地自动文档生成。

---

## 6. 结论

- **定义**：以字段级 DSL（`RegFieldDef`）为单一事实源，`GenBundle` 风格作为快捷入口并桥接到同一 IR；访问类型下沉到字段级。
- **连接**：保留 per-type 寄存器模块的 `wrEn/wrData` 脉冲语义（对外围逻辑最友好），连接层从字符串键升级为生成的类型安全视图，RO 一律用命名输入端口。
- **生成**：一个 IR 三路输出 —— RTL（`RegFileTop`）、软件视图（JSON/C）、文档（Markdown/HTML 位域图，新增）。
- **优先修**：GeneratorLib 删除导致的构建风险、RO 读回恒 0、AXI 包装器 RO 未驱动三个问题。
