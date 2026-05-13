# RegCbb 设计文档

## 1. 概述

RegCbb (Register Circuit Building Blocks) 是一个用于构建硬件寄存器接口的 Chisel 库，支持：

- 多种寄存器类型 (RW, RO, WO, RC, RS, W1C, W1S, W1T)
- AXI4 / AXI4-Lite 总线接口
- 灵活的 DSL 风格寄存器定义
- 自动地址分配
- 寄存器描述信息生成 (JSON, C头文件)

## 2. 目录结构

```
RegCbb/
├── RegType.scala           # 核心寄存器类型定义
├── RegFieldDescSer.scala   # 寄存器字段描述序列化
├── AxiInterfaces.scala     # AXI总线接口定义
├── dsl/
│   ├── RegisterBlockDsl.scala   # 寄存器块DSL定义
│   ├── RegFieldDsl.scala        # 寄存器字段DSL
│   ├── MemoryDef.scala          # 存储器定义
│   ├── AddressAllocator.scala   # 地址分配器
│   ├── RegisterIRGenerator.scala  # IR生成器(生成JSON/C头文件)
│   ├── RegisterFileGenerator.scala # 寄存器文件生成器
│   ├── GenRegBlock.scala         # GenBundle风格寄存器块
│   └── example/                 # 使用示例
```

## 3. 核心寄存器类型

### 3.1 寄存器类型 (RegType.scala)

| 类型 | 描述 | 核心接口信号 |
|------|------|-------------|
| **RW** | 读写寄存器 | `wrEn`, `wrData` |
| **RO** | 只读寄存器 | `wrData` (用户驱动) |
| **WO** | 只写寄存器 | `wrEn`, `wrData` |
| **RC** | 读清寄存器 | `rdData`, `rdEn` |
| **RS** | 读置寄存器 | `rdData`, `rdEn` |
| **W1C** | 写1清零 | `wrEn`, `wrData` |
| **W1S** | 写1置位 | `wrEn`, `wrData` |
| **W1T** | 写1翻转 | `wrEn`, `wrData` |

### 3.2 核心接口

每个寄存器模块包含两个接口：

**Decoder接口 (dec)**:
```scala
class dec_if[T <: Data](gen: T = UInt(32.W)) extends Bundle {
  val in = new dec_in(gen)    // wr, wdata, rd 输入
  val out = new dec_out(gen)  // rdata 输出
}
```

**Core接口 (core)**:
```scala
// RW/WO/W1C/W1S/W1T 使用
class rw_core_if(info: RegInfo) extends Bundle {
  val wrEn = Output(Bool())
  val wrData = Output(info.DataType)
}

// RO 使用
class ro_core_if(info: RegInfo) extends Bundle {
  val wrData = Flipped(Output(info.DataType))
}

// RC/RS 使用
class rc_core_if(info: RegInfo) extends Bundle {
  val rdData = Output(info.DataType)
  val rdEn = Output(Bool())
}
```

## 4. AXI总线接口

### 4.1 AXI4 Lite (AxiLiteBusIO)

```scala
class AxiLiteBusIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  // 写地址通道
  val aw_valid, aw_ready = Bool()
  val aw_addr = UInt(addrWidth.W)
  val aw_prot = UInt(3.W)

  // 写数据通道
  val w_valid, w_ready = Bool()
  val w_data = UInt(dataWidth.W)
  val w_strb = UInt((dataWidth/8).W)

  // 写响应通道
  val b_valid, b_ready = Bool()
  val b_resp = UInt(2.W)

  // 读地址通道
  val ar_valid, ar_ready = Bool()
  val ar_addr = UInt(addrWidth.W)
  val ar_prot = UInt(3.W)

  // 读数据通道
  val r_valid, r_ready = Bool()
  val r_data = UInt(dataWidth.W)
  val r_resp = UInt(2.W)
}
```

### 4.2 AXI4 Full (AxiBusIO)

支持Burst操作，额外字段包括: `aw_len`, `aw_size`, `aw_burst`, `aw_lock`, `aw_cache`, `aw_qos`, `w_last`, `r_last` 等。

## 5. DSL风格定义

### 5.1 字段定义 (RegFieldDsl.scala)

```scala
import RegField._

// 定义字段
val enableField = RegField("enable", 1) { b =>
  b.rw().reset(0).desc("Enable bit")
}

val modeField = RegField("mode", 2) { b =>
  b.rw().reset(0).desc("Mode select")
  b.oneToClear()  // 写1清零
}

// 快捷方式
val dataField = RegField.rw("data", 32, 0, "Data register")
val statusField = RegField.ro("status", 8, 0, "Status register")
```

### 5.2 寄存器块定义 (RegisterBlockDsl.scala)

```scala
import RegBlock._

val regBlock = RegBlock("myDevice") { b =>
  b.baseAddress(0x1000)
  b.desc("My Register Block")

  b.reg("control") { r =>
    r.field(RegField("enable", 1) { b => b.rw() })
    r.field(RegField("mode", 2) { b => b.rw() })
    r.desc("Control register")
    r.group("control")
  }

  b.reg("status") { r =>
    r.field(RegField("busy", 1) { b => b.ro() })
    r.field(RegField("error", 1) { b => b.ro() })
    r.desc("Status register")
  }

  b.mem("memory") { m =>
    m.depth(1024).dataWidth(32).sp()
    m.baseAddress(0x2000)
    m.desc("SRAM memory")
  }
}
```

### 5.3 存储器定义 (MemoryDef.scala)

```scala
import MemoryDef._

// 单端口SRAM
val sram = MemoryDef.sp("sram", depth = 1024, dataWidth = 32, baseAddress = 0x2000) { m =>
  m.desc("Data memory")
}

// 双端口SRAM
valuram twoPortRam = MemoryDef.tp("dpRam", depth = 512, dataWidth = 64) { m =>
  m.desc("Two-port RAM")
}
```

## 6. 地址分配

### 6.1 AddressAllocator

地址分配器为寄存器块中的寄存器和存储器分配地址：

- 寄存器按4字节对齐
- 支持多字节寄存器跨word边界
- 字段在寄存器内LSB优先排列

```scala
case class RegisterAllocation(
  register: RegDef,
  byteOffset: BigInt,      // 字节偏移
  byteSize: Int,            // 字节大小
  wordOffset: Int,          // 起始32bit word索引
  wordCount: Int,           // 占用的word数量
  fieldAllocations: Seq[FieldAllocation]
)
```

## 7. 寄存器文件生成

### 7.1 RegisterFileGenerator

生成完整的寄存器文件模块：

```scala
// 生成寄存器文件和存储
val (regIO, regDataOut, memPortIO) = RegisterFileGenerator.generate(addressMap)
```

功能：
- 寄存器存储
- 地址解码
- 写逻辑处理 (支持多种写操作)
- 读逻辑处理 (支持RC自动清零)
- 存储器实例化
- 直接寄存器输出

### 7.2 RegisterIRGenerator

生成描述信息用于软件/文档：

```scala
// 生成IR
val ir = RegisterIRGenerator.generate(addressMap)

// 输出JSON
val json = RegisterIRGenerator.toJson(ir)

// 输出C头文件
val header = RegisterIRGenerator.toCHeader(ir)
```

生成内容：
- 寄存器字段描述 (名称、位宽、偏移、访问类型、复位值)
- 寄存器地址映射
- 存储器地址映射
- C宏定义 (#define)

## 8. GenBundle风格 (GenRegBlock.scala)

### 8.1 核心概念

使用 `GenBundle` 定义整个模块的寄存器集合，每个顶层元素对应一个独立寄存器模块。

### 8.2 命名约定

| 命名模式 | 访问类型 |
|----------|----------|
| `<name>_ro` | RO (只读) |
| `<name>_wo` | WO (只写) |
| `<name>_rc` | RC (读清) |
| `<name>_rs` | RS (读置) |
| `<name>_w1c` | W1C (写1清) |
| `<name>_w1s` | W1S (写1置) |
| `<name>_w1t` | W1T (写1翻) |
| `<name>` (无后缀) | RW (读写) |

### 8.3 使用示例

```scala
// 定义寄存器接口
class MyRegInterface extends GenBundle {
  val ctrl = new GenBundle {
    val enable = Bool()    // RW: enable
    val mode = UInt(2.W)   // RW: mode
    val start = Bool()      // RW: start
  }
  val status = new GenBundle {
    val busy = Bool()      // RW: busy
    val done = Bool()      // RW: done
  }
  val data = UInt(32.W)    // RW: data
}

// 创建寄存器块
val regBlock = Module(new GenRegBlock.RegBlock(regIf))

// 连接
regBlock.io.wr := wrEn
regBlock.io.rd := rdEn
regBlock.io.addr := addr
regBlock.io.wdata := wdata

// 访问核心接口
val ctrlCore = regBlock.io.cores(0)
when(ctrlCore.wrEn) {
  // 处理写
}
```

### 8.4 AXI Lite寄存器块

```scala
class AxiLiteRegBlock(bundle: GenBundle, addrWidth: Int = 12, dataWidth: Int = 32)
    extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AxiLiteBusIO(addrWidth, dataWidth))
    val regs = Output(Vec(regCount, new RegCoreOutput(32)))
  })
  // ...
}
```

## 9. 设计特点

### 9.1 分离关注点

- **寄存器类型**: 每种寄存器类型独立模块，简化用户逻辑
- **地址解码**: 集中管理地址解码逻辑
- **总线接口**: AXI适配器与寄存器块分离

### 9.2 灵活的接口信号

用户可根据需要选择：
- `wrEn`/`wrData`: 捕获写操作
- `rdData`/`rdEn`: 捕获读操作
- 直接读写寄存器值

### 9.3 自动化工具链

1. **地址分配**: 自动计算寄存器/存储器布局
2. **IR生成**: 生成JSON描述文件
3. **C头文件**: 生成软件头文件
4. **Verilog**: Chisel编译生成Verilog

### 9.4 支持的写操作

| 写操作 | 行为 |
|--------|------|
| Normal | 直接写入 |
| OneToClear | 写1清零 |
| OneToSet | 写1置位 |
| OneToToggle | 写1翻转 |
| ClearOnRead | 读后清零 |

## 10. 数据流

```
软件/CPU                    硬件
    |                         |
    | ---- AXI Write ------>  |
    |                         | +--> RegBlock
    |                         |      +--> rw/wo/ro/rc/rs/w1c/w1s/w1t
    |                         |      |        |
    |                         |      |        +--> user logic (wrEn/wrData)
    |                         |      |
    | <---- AXI Response ---- |      |
    |                         |
    | ---- AXI Read ------->  |
    |                         | +--> RegBlock
    |                         |      +--> user logic (rdData/rdEn)
    |                         |
    | <---- AXI Read Data --- |
    |                         |
```

## 11. 使用场景

1. **IP核寄存器接口**: 快速定义标准寄存器接口
2. **SoC外设**: 构建外设寄存器映射
3. **测试框架**: 生成可配置的寄存器模型
4. **自动化工具链**: JSON/C头文件支持软件驱动生成
