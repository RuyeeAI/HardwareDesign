package BaseCbb.RegCbb.demo

import chisel3._
import chisel3.util.Cat
import BaseCbb.RegCbb._
import BaseCbb.RegCbb.dsl._
import BaseCbb.RegCbb.hw._
import BaseCbb.RegCbb.gen._

/**
 * ==================== RegCbb_v2 设计 Demo ====================
 *
 * 一个简化的 UART 外设，演示完整的"定义 → 分配 → 生成 → 连接"流程：
 *   1. UartDemoDef.build  —— 用字段级 DSL + RegBundle（GenBundle 风格）定义寄存器（单一事实源）
 *   2. AddressAllocator   —— 自动分配地址
 *   3. RegFileTop         —— 生成 RTL（简单总线，寄存器 + memory 地址空间统一解码）
 *   4. RegView            —— 用户逻辑连接（命名访问 + 自动位域切割）
 *   5. EmitAll            —— 一键生成 Verilog / JSON / C 头 / Markdown / HTML / 具名视图
 *
 * 寄存器集合（演示覆盖所有典型场景）：
 *   ctrl        RW + RO 混排字段（字段级访问类型）
 *   status_ro   RO 寄存器（用户驱动，软件读回 —— v2 已修复 v1 读回恒 0 的 bug）
 *   tx_data_wo  WO 寄存器（写脉冲捕获）
 *   rx_data_ro  RO 寄存器
 *   irq_w1c     W1C 寄存器（硬件置位 + 软件写 1 清除）
 *   irq_en      RW 寄存器（中断使能）
 *   scratch     32bit RW
 *   data64      ★ 64bit RW 原子寄存器（word 间大端：低地址=高有效 word，写 +0x1C 提交）
 *   data64_plain ★ 64bit RW 非原子寄存器（逐字直接写）
 *   bundle_*    ★ RegBundle（GenBundle 风格）定义的寄存器组
 *   tx_fifo     ★ 64bit 宽 memory 地址空间（总线原子访问 + 外部 SRAM 接口）
 */
object UartDemoDef {

  /** RegBundle（GenBundle 风格）定义的寄存器组：嵌套 RegBundle=寄存器、叶子=单字段寄存器 */
  class UartBundleRegs extends RegBundle {
    val bundle_ctrl = new RegBundle {
      val mode  = UInt(2.W)
      val burst = Bool()
      Attr += (mode  -> FieldAttr("工作模式", reset = 1))
      Attr += (burst -> FieldAttr("突发使能"))
    }
    val bundle_status_ro = new RegBundle {
      val link_up = Bool()
      Attr += (link_up -> FieldAttr("链路状态"))
    }
    val bundle_scratch_ro = UInt(8.W)   // 叶子元素 = 单字段寄存器（_ro 后缀 → RO）
  }

  /**
   * ★ RegBundle 定义 memory entry 域段：
   * 嵌套 RegBundle = entry 的一组连续域段（LSB-first 紧凑），叶子 = 单域段。
   * 位宽和 = entry 位宽（此处 8+16+8 = 32bit，即 MemoryDef.dataWidth）。
   */
  class FifoDescEntry extends RegBundle {
    val desc = new RegBundle {
      val tag = UInt(8.W)
      val len = UInt(16.W)
      Attr += (tag -> FieldAttr("描述标签"))
      Attr += (len -> FieldAttr("数据长度"))
    }
    val crc = UInt(8.W)
    Attr += (crc -> FieldAttr("CRC 校验"))
  }

  /**
   * 定义（新系统级 DSL）：
   *  - RegBlock：纯寄存器块（功能片段）
   *  - MemBlock：纯存储器块（与寄存器块分离）
   *  - Module：功能模块 = 多个 RegBlock + 多个 MemBlock
   *  - System：系统 = 多个功能模块（地址自动/手工分配）
   */
  def build: SystemDef = System("uart_system") { s =>
    s.device("UART")
    s.desc("RegCbb 演示系统：UART 功能模块")

    s.module(FuncModule("uart") { m =>
      m.baseAddress(0x40000000L)      // 模块基址手工指定
      m.memBaseAddress(0x40001000L)   // 模块存储器区手工指定
      m.desc("简化 UART 外设")

      // ---- RegBlock 1：控制/状态寄存器组 ----
      m.regBlock(RegBlock("ctrl_regs") { b =>
        b.reg("ctrl") { r =>
          r.desc("控制寄存器")
          r.field(RegField.rw("tx_en", 1, 0, "发送使能"))
          r.field(RegField.rw("baud_div", 12, 4, "波特率分频值（4..4095）"))
          r.field(RegField.ro("version", 4, 0, "硬件版本号"))       // 字段级 RO（混排在 RW 寄存器内）
        }

        b.reg("status_ro") { r =>
          r.desc("状态寄存器（只读）")
          r.field(RegField.ro("tx_busy", 1, 0, "发送忙（电平）"))
          r.field(RegField.ro("tx_done", 1, 0, "发送完成（电平）"))
        }

        b.reg("tx_data_wo") { r =>
          r.desc("发送数据寄存器（只写）")
          r.field(RegField.wo("data", 8, "待发送字节"))
        }

        b.reg("rx_data_ro") { r =>
          r.desc("接收数据寄存器（只读）")
          r.field(RegField.ro("data", 8, "最近接收的字节"))
        }

        b.reg("irq_w1c") { r =>
          r.desc("中断状态寄存器（写 1 清除，硬件置位）")
          r.field(RegField.w1c("tx_done", 1, "发送完成中断"))
          r.field(RegField.w1c("rx_rdy", 1, "接收就绪中断"))
        }

        b.reg("irq_en") { r =>
          r.desc("中断使能寄存器")
          r.field(RegField.rw("tx_done", 1, 0, "发送完成中断使能"))
          r.field(RegField.rw("rx_rdy", 1, 0, "接收就绪中断使能"))
        }
      })

      // ---- RegBlock 2：数据/测试寄存器组 ----
      m.regBlock(RegBlock("data_regs") { b =>
        b.reg("scratch") { r =>
          r.desc("测试寄存器")
          r.field(RegField("value", 32) { f =>
            f.rw().reset(0xDEADBEEFL).desc("任意读写，用于冒烟测试")
          })
        }

        b.reg("data64") { r =>
          r.desc("64 位原子寄存器：word 间大端（低地址=高有效 word），写 +0x1C 提交")
          r.atomic()
          r.field(RegField.rw("value", 64, 0, "64 位数据（原子访问）"))
        }

        b.reg("data64_plain") { r =>
          r.desc("64 位非原子寄存器：word 间大端（+0x24=高 word，+0x28=低 word）逐字直接写")
          r.nonAtomic()
          r.field(RegField.rw("value", 64, 0, "64 位数据（非原子访问）"))
        }

        // ★ 规则 3 演示：40bit 寄存器（>32bit → 占据 64bit = 2 words，有效数据从高 bit 位开始放）
        b.reg("data40") { r =>
          r.desc("40 位原子寄存器：占据 64bit（2 的幂扩展），有效数据在 bit[63:24]，低 24 位 padding 0")
          r.atomic()
          r.field(RegField.rw("value", 40, 0, "40 位数据（高 bit 放置）"))
        }

        // ★ RegBundle（GenBundle 风格）定义的寄存器组
        b.regs(BundleToRegDefs.toRegDefs(new UartBundleRegs))
      })

      // ---- MemBlock：存储器块（与寄存器块分离） ----
      m.memBlock(MemBlock("uart_mems") { mb =>
        mb.mem("tx_fifo") { mm =>
          mm.depth(64).dataWidth(64).sp()
            .atomic()
            .desc("64 位宽发送 FIFO 存储（原子访问）")
        }

        mb.mem("tx_fifo_plain") { mm =>
          mm.depth(64).dataWidth(64).sp()
            .nonAtomic()
            .desc("64 位宽普通存储（非原子：逐字读-改-写）")
        }

        // ★ 规则 3 演示：96bit memory（>32bit → 占据 128bit = 4 words，有效数据从高 bit 位开始放）
        mb.mem("wide_mem") { mm =>
          mm.depth(16).dataWidth(96).sp()
            .atomic()
            .desc("96 位宽存储（规则 3：占据 128bit 地址空间，有效数据在 bit[127:32]，低 32 位 padding 0）")
        }

        // ★ Memory entry 域段来自 RegBundle：位宽与字段布局自动推导
        mb.mem("rx_desc") { mm =>
          mm.depth(32).sp().atomic()
            .bundle(new FifoDescEntry)
            .desc("接收描述符表（32bit entry = tag[7:0] | len[23:8] | crc[31:24]，域段来自 RegBundle）")
        }
      })
    })

    // ---- 第二个功能模块：GPIO（演示多模块 + 自动地址分配） ----
    s.module(FuncModule("gpio") { m =>
      m.desc("GPIO 演示模块（自动地址分配）")
      m.regBlock(RegBlock("gpio_regs") { b =>
        b.reg("gpio_ctl") { r =>
          r.desc("GPIO 控制")
          r.field(RegField.rw("dir", 8, 0, "方向（1=输出）"))
          r.field(RegField.rw("out", 8, 0, "输出值"))
          r.field(RegField.ro("in", 8, 0, "输入值"))
        }
        b.reg("gpio_irq") { r =>
          r.desc("GPIO 中断")
          r.field(RegField.w1c("rise", 8, "上升沿中断"))
          r.field(RegField.w1c("fall", 8, "下降沿中断"))
        }
      })
    })
  }
}

/**
 * UART 演示模块（简单总线版本）。
 *
 * 外围逻辑连接要点（★ = 统一 Bundle / 类型化子接口写法，推荐）：
 *  - 写侧（SW→HW）：★ `regs("tx_data_wo").sw.wrEn` 写脉冲；字段数据用 `.field("data").wrData` 自动位域切割
 *  - 读当前值：★ `regs("ctrl").sw.value`（全宽）；字段切割用 `.field("baud_div").value`
 *  - RO 驱动：★ `regs("status_ro").ro.value("tx_busy") := txBusy`（ro 子接口按字段名取端口）
 *  - 中断置位：★ `regs("irq_w1c").hwSet.bits("tx_done") := txDonePulse`（hwSet 子接口）
 *  - RW 硬件直写：★ `regs("data64").hwWr.en := ...` / `.hwWr.data("value") := ...`
 *  - 旧便捷写法（`.wrEn`、`.field("x").roValue`）仍可用（RegHandle/FieldHandle 委托保持兼容）
 */
class UartDemo extends Module {
  val io = IO(new Bundle {
    val wr    = Input(Bool())
    val rd    = Input(Bool())
    val addr  = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
    val tx    = Output(Bool())
    val irq   = Output(Bool())
  })

  private val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
  private val uartAlloc = sysMap.moduleByName("uart")
  private val map = uartAlloc.toRegFileMap
  private val regFile = Module(new RegFileTop(map))
  private val regs = RegView(map, regFile)

  // ---- 简单总线 ----
  regFile.io.wr := io.wr
  regFile.io.rd := io.rd
  regFile.io.addr := io.addr
  regFile.io.wdata := io.wdata
  io.rdata := regFile.io.rdata

  // ---- 发送状态机（外围逻辑示例） ----
  val txBusy      = RegInit(false.B)
  val txDonePulse = WireDefault(false.B)      // 单周期脉冲 → 置位 W1C / RO 电平
  val txShift     = RegInit(0.U(10.W))        // {stop, data[7:0], start}
  val baudCnt     = RegInit(0.U(16.W))
  val bitIdx      = RegInit(0.U(4.W))
  // 字段级位域切割（baud_div 占 bit[13:2]）；全宽等价写法：regs("ctrl").sw.value(17, 2)
  val baudDiv = regs("ctrl").field("baud_div").value

  // ★ sw 子接口写事件：wrEn/wrData 同拍（全宽 wrData，data 字段占 bit[7:0]）
  val txWo = regs("tx_data_wo")
  when(txWo.sw.wrEn) {
    txShift := Cat(1.U(1.W), txWo.field("data").wrData, 0.U(1.W)) // 字段数据：自动位域切割
    bitIdx  := 0.U
    txBusy  := true.B
  }
  when(txBusy) {
    baudCnt := baudCnt + 1.U
    when(baudCnt === baudDiv) {
      baudCnt := 0.U
      when(bitIdx === 9.U) {                  // 第 10 拍：停止位发送完毕
        txBusy := false.B
        txDonePulse := true.B
      }.otherwise {
        bitIdx := bitIdx + 1.U
        txShift := Cat(1.U, txShift(9, 1))    // 右移输出
      }
    }
  }
  io.tx := Mux(txBusy, txShift(0), true.B)    // 空闲高电平

  // ---- RO 驱动（用户逻辑 → 寄存器）：★ ro 子接口，按字段名的独立端口 ----
  val statusRo = regs("status_ro")
  statusRo.ro.value("tx_busy") := txBusy
  statusRo.ro.value("tx_done") := RegNext(txDonePulse, false.B)
  regs("ctrl").ro.value("version")      := 2.U(4.W)
  regs("rx_data_ro").ro.value("data")   := 0x5A.U(8.W) // 演示固定值

  // ---- W1C 硬件置位：★ hwSet 子接口 ----
  val irqW1c = regs("irq_w1c")
  irqW1c.hwSet.bits("tx_done") := txDonePulse
  irqW1c.hwSet.bits("rx_rdy")  := false.B

  // ---- RegBundle 寄存器连接 ----
  val linkUp = RegInit(false.B)
  val bundleCtrl = regs("bundle_ctrl")
  when(bundleCtrl.sw.wrEn) { linkUp := bundleCtrl.field("burst").wrData(0) }
  regs("bundle_status_ro").ro.value("link_up") := linkUp
  regs("bundle_scratch_ro").ro.value("bundle_scratch_ro") := 0x7.U(8.W) // 演示：RO 由硬件驱动

  // ---- 64bit 原子寄存器：★ hwWr 子接口硬件直写演示 ----
  regs("data64").hwWr.en := false.B  // 默认不写（SW 经 0x1c/0x20 原子访问）；字段直写：.hwWr.data("value") := ...

  // ---- Memory 地址空间：外部 64bit SRAM 挂接（请求-响应协议）----
  // 用户侧逻辑：零延迟响应 —— rd/wr 拉高即获得带宽，同拍返回 ack；status=OK
  val sram = Mem(64, UInt(64.W))
  sram.suggestName("ext_tx_fifo_sram")
  val mp = regFile.io.memPorts.elements("tx_fifo").asInstanceOf[MemPortIO]
  when(mp.wr) { sram.write(mp.waddr, mp.wdata) }
  mp.rdata := sram.read(mp.raddr)
  mp.ack := mp.rd || mp.wr
  mp.status := MemStatus.OK

  val sramPlain = Mem(64, UInt(64.W))
  sramPlain.suggestName("ext_tx_fifo_plain_sram")
  val mpPlain = regFile.io.memPorts.elements("tx_fifo_plain").asInstanceOf[MemPortIO]
  when(mpPlain.wr) { sramPlain.write(mpPlain.waddr, mpPlain.wdata) }
  mpPlain.rdata := sramPlain.read(mpPlain.raddr)
  mpPlain.ack := mpPlain.rd || mpPlain.wr
  mpPlain.status := MemStatus.OK

  // rx_desc：32bit（域段来自 RegBundle）
  val descSram = Mem(32, UInt(32.W))
  descSram.suggestName("ext_rx_desc_sram")
  val mpDesc = regFile.io.memPorts.elements("rx_desc").asInstanceOf[MemPortIO]
  when(mpDesc.wr) { descSram.write(mpDesc.waddr, mpDesc.wdata) }
  mpDesc.rdata := descSram.read(mpDesc.raddr)
  mpDesc.ack := mpDesc.rd || mpDesc.wr
  mpDesc.status := MemStatus.OK

  // wide_mem：96bit 数据 → 占据 128bit（数据在 bit[127:32]）
  val wideSram = Mem(16, UInt(128.W))
  wideSram.suggestName("ext_wide_mem_sram")
  val mpWide = regFile.io.memPorts.elements("wide_mem").asInstanceOf[MemPortIO]
  when(mpWide.wr) { wideSram.write(mpWide.waddr, mpWide.wdata) }
  mpWide.rdata := wideSram.read(mpWide.raddr)
  mpWide.ack := mpWide.rd || mpWide.wr
  mpWide.status := MemStatus.OK

  // ---- 中断输出 = 中断状态 & 使能（★ sw 子接口读全宽当前值） ----
  val irqStatus = regs("irq_w1c").sw.value
  val irqEn     = regs("irq_en").sw.value
  io.irq := (irqStatus(0) && irqEn(0)) || (irqStatus(1) && irqEn(1))

  // 控制台打印地址布局
  println(AddressAllocator.summarize(map))
}

/** AXI4-Lite 版本（同一份定义，换一个包装器） */
class UartAxiDemo extends Module {
  val io = IO(new Bundle {
    val axi = new AxiLiteBusIO(32, 32)
    val tx  = Output(Bool())
    val irq = Output(Bool())
  })

  private val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
  private val uartAlloc = sysMap.moduleByName("uart")
  private val map = uartAlloc.toRegFileMap
  private val regFile = Module(new AxiLiteRegFile(map))
  private val regs = RegView(map, regFile)

  // AXI 中继（从设备视角，方向逐信号连接）
  regFile.io.axi.aw_valid := io.axi.aw_valid
  regFile.io.axi.aw_addr  := io.axi.aw_addr
  regFile.io.axi.aw_prot  := io.axi.aw_prot
  io.axi.aw_ready := regFile.io.axi.aw_ready
  regFile.io.axi.w_valid := io.axi.w_valid
  regFile.io.axi.w_data  := io.axi.w_data
  regFile.io.axi.w_strb  := io.axi.w_strb
  io.axi.w_ready := regFile.io.axi.w_ready
  io.axi.b_valid := regFile.io.axi.b_valid
  io.axi.b_resp  := regFile.io.axi.b_resp
  regFile.io.axi.b_ready := io.axi.b_ready
  regFile.io.axi.ar_valid := io.axi.ar_valid
  regFile.io.axi.ar_addr  := io.axi.ar_addr
  regFile.io.axi.ar_prot  := io.axi.ar_prot
  io.axi.ar_ready := regFile.io.axi.ar_ready
  io.axi.r_valid := regFile.io.axi.r_valid
  io.axi.r_data  := regFile.io.axi.r_data
  io.axi.r_resp  := regFile.io.axi.r_resp
  regFile.io.axi.r_ready := io.axi.r_ready

  // 演示：硬件持续置位"接收就绪"中断位（★ hwSet 子接口）
  regs("irq_w1c").hwSet.bits("rx_rdy") := true.B
  io.tx := true.B
  io.irq := regs("irq_w1c").sw.value(1) && regs("irq_en").sw.value(1)

  // 外部 64bit SRAM 挂接（AXI 总线可经 0x40001000 原子访问；请求-响应协议）
  val sram = Mem(64, UInt(64.W))
  sram.suggestName("ext_tx_fifo_sram")
  val mp = regFile.io.memPorts.elements("tx_fifo").asInstanceOf[MemPortIO]
  when(mp.wr) { sram.write(mp.waddr, mp.wdata) }
  mp.rdata := sram.read(mp.raddr)
  mp.ack := mp.rd || mp.wr
  mp.status := MemStatus.OK

  val sramPlain = Mem(64, UInt(64.W))
  sramPlain.suggestName("ext_tx_fifo_plain_sram")
  val mpPlain = regFile.io.memPorts.elements("tx_fifo_plain").asInstanceOf[MemPortIO]
  when(mpPlain.wr) { sramPlain.write(mpPlain.waddr, mpPlain.wdata) }
  mpPlain.rdata := sramPlain.read(mpPlain.raddr)
  mpPlain.ack := mpPlain.rd || mpPlain.wr
  mpPlain.status := MemStatus.OK

  // rx_desc：32bit
  val descSram = Mem(32, UInt(32.W))
  descSram.suggestName("ext_rx_desc_sram")
  val mpDesc = regFile.io.memPorts.elements("rx_desc").asInstanceOf[MemPortIO]
  when(mpDesc.wr) { descSram.write(mpDesc.waddr, mpDesc.wdata) }
  mpDesc.rdata := descSram.read(mpDesc.raddr)
  mpDesc.ack := mpDesc.rd || mpDesc.wr
  mpDesc.status := MemStatus.OK

  // wide_mem：96bit 数据 → 占据 128bit
  val wideSram = Mem(16, UInt(128.W))
  wideSram.suggestName("ext_wide_mem_sram")
  val mpWide = regFile.io.memPorts.elements("wide_mem").asInstanceOf[MemPortIO]
  when(mpWide.wr) { wideSram.write(mpWide.waddr, mpWide.wdata) }
  mpWide.rdata := wideSram.read(mpWide.raddr)
  mpWide.ack := mpWide.rd || mpWide.wr
  mpWide.status := MemStatus.OK
}

/**
 * 系统级演示：一个系统 = 多个功能模块（UART + GPIO），
 * 演示 SystemRegFileTop 的模块间地址译码分发汇聚 + SystemRegView 三级命名访问。
 */
class UartSystemDemo extends Module {
  val io = IO(new Bundle {
    val wr    = Input(Bool())
    val rd    = Input(Bool())
    val addr  = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
    val gpio  = Output(UInt(8.W))
  })

  private val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
  private val sysRegFile = Module(new SystemRegFileTop(sysMap))
  private val sysRegs = SystemRegView(sysMap, sysRegFile)

  // ---- 系统总线 ----
  sysRegFile.io.wr := io.wr
  sysRegFile.io.rd := io.rd
  sysRegFile.io.addr := io.addr
  sysRegFile.io.wdata := io.wdata
  io.rdata := sysRegFile.io.rdata

  // ---- 模块/块/寄存器三级访问（SystemRegView） ----
  // UART 模块：ctrl_regs 块中的 ctrl
  val uartCtrl = sysRegs.module("uart").block("ctrl_regs").reg("ctrl")
  // GPIO 模块：gpio_regs 块
  val gpioCtl = sysRegs.module("gpio").block("gpio_regs").reg("gpio_ctl")
  val gpioIrq = sysRegs.module("gpio").block("gpio_regs").reg("gpio_irq")

  // GPIO 输出 = out 字段 & dir 字段（演示跨模块寄存器读取）
  io.gpio := gpioCtl.field("out").value & gpioCtl.field("dir").value

  // UART tx 演示：写 tx_data_wo 触发（简化，无真实串口）；★ sw / ro 子接口
  val txBusy = RegInit(false.B)
  when(sysRegs.reg("tx_data_wo").sw.wrEn) { txBusy := true.B }
  when(txBusy) { txBusy := false.B } // 简化：1 拍完成
  sysRegs.module("uart").reg("status_ro").ro.value("tx_busy") := txBusy

  // 硬件置位 GPIO 中断：★ hwSet 子接口
  gpioIrq.hwSet.bits("rise") := RegNext(gpioCtl.field("in").value & ~RegNext(gpioCtl.field("in").value), false.B)

  // 外部 SRAM 挂接（uart 模块的 tx_fifo / tx_fifo_plain）
  val sram = Mem(64, UInt(64.W))
  sram.suggestName("ext_sys_tx_fifo_sram")
  val mp = sysRegFile.io.memPorts.elements("tx_fifo").asInstanceOf[MemPortIO]
  when(mp.wr) { sram.write(mp.waddr, mp.wdata) }
  mp.rdata := sram.read(mp.raddr)
  mp.ack := mp.rd || mp.wr
  mp.status := MemStatus.OK

  val sramPlain = Mem(64, UInt(64.W))
  sramPlain.suggestName("ext_sys_tx_fifo_plain_sram")
  val mpPlain = sysRegFile.io.memPorts.elements("tx_fifo_plain").asInstanceOf[MemPortIO]
  when(mpPlain.wr) { sramPlain.write(mpPlain.waddr, mpPlain.wdata) }
  mpPlain.rdata := sramPlain.read(mpPlain.raddr)
  mpPlain.ack := mpPlain.rd || mpPlain.wr
  mpPlain.status := MemStatus.OK

  // rx_desc：32bit entry（域段来自 RegBundle），零延迟响应
  val descSram = Mem(32, UInt(32.W))
  descSram.suggestName("ext_sys_rx_desc_sram")
  val mpDesc = sysRegFile.io.memPorts.elements("rx_desc").asInstanceOf[MemPortIO]
  when(mpDesc.wr) { descSram.write(mpDesc.waddr, mpDesc.wdata) }
  mpDesc.rdata := descSram.read(mpDesc.raddr)
  mpDesc.ack := mpDesc.rd || mpDesc.wr
  mpDesc.status := MemStatus.OK

  // wide_mem：96bit 数据 → 占据 128bit（MemPortIO.dataWidth=128，有效数据在 bit[127:32]）
  val wideSram = Mem(16, UInt(128.W))
  wideSram.suggestName("ext_sys_wide_mem_sram")
  val mpWide = sysRegFile.io.memPorts.elements("wide_mem").asInstanceOf[MemPortIO]
  when(mpWide.wr) { wideSram.write(mpWide.waddr, mpWide.wdata) }
  mpWide.rdata := wideSram.read(mpWide.raddr)
  mpWide.ack := mpWide.rd || mpWide.wr
  mpWide.status := MemStatus.OK

  println(sysMap.summarize)
}

/** AXI4-Lite 系统级版本 */
class UartSystemAxiDemo extends Module {
  val io = IO(new Bundle {
    val axi  = new AxiLiteBusIO(32, 32)
    val gpio = Output(UInt(8.W))
  })

  private val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
  private val sysRegFile = Module(new SystemAxiLiteRegFile(sysMap))
  private val sysRegs = SystemRegView(sysMap, sysRegFile)

  // AXI 中继
  sysRegFile.io.axi.aw_valid := io.axi.aw_valid
  sysRegFile.io.axi.aw_addr  := io.axi.aw_addr
  sysRegFile.io.axi.aw_prot  := io.axi.aw_prot
  io.axi.aw_ready := sysRegFile.io.axi.aw_ready
  sysRegFile.io.axi.w_valid := io.axi.w_valid
  sysRegFile.io.axi.w_data  := io.axi.w_data
  sysRegFile.io.axi.w_strb  := io.axi.w_strb
  io.axi.w_ready := sysRegFile.io.axi.w_ready
  io.axi.b_valid := sysRegFile.io.axi.b_valid
  io.axi.b_resp  := sysRegFile.io.axi.b_resp
  sysRegFile.io.axi.b_ready := io.axi.b_ready
  sysRegFile.io.axi.ar_valid := io.axi.ar_valid
  sysRegFile.io.axi.ar_addr  := io.axi.ar_addr
  sysRegFile.io.axi.ar_prot  := io.axi.ar_prot
  io.axi.ar_ready := sysRegFile.io.axi.ar_ready
  io.axi.r_valid := sysRegFile.io.axi.r_valid
  io.axi.r_data  := sysRegFile.io.axi.r_data
  io.axi.r_resp  := sysRegFile.io.axi.r_resp
  sysRegFile.io.axi.r_ready := io.axi.r_ready

  val gpioCtl = sysRegs.module("gpio").block("gpio_regs").reg("gpio_ctl")
  io.gpio := gpioCtl.field("out").value & gpioCtl.field("dir").value
}

/** 一键生成所有产物：sbt "runMain BaseCbb.RegCbb.demo.EmitAll [outDir]" */
object EmitAll extends App {
  private val outDir = args.headOption.getOrElse("generated/RegCbb")
  private val dir = new java.io.File(outDir)
  dir.mkdirs()

  private def write(name: String, content: String): Unit = {
    val f = new java.io.File(dir, name)
    val w = new java.io.PrintWriter(f)
    try w.write(content) finally w.close()
    println(s"written: ${f.getPath} (${content.length} chars)")
  }

  // ---- 系统级定义与分配 ----
  private val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
  println(sysMap.summarize)

  // ---- 系统级文档（不依赖外围逻辑，纯 IR 生成） ----
  write("system.json",  SystemJsonGen.generate(sysMap))
  write("system.h",     SystemCHeaderGen.generate(sysMap))
  write("system.md",    SystemMarkdownGen.generate(sysMap))
  write("system.html",  SystemHtmlGen.generate(sysMap))
  write("SystemRegs.scala", SystemViewSourceGen.generate(sysMap, "SystemRegs", "BaseCbb.RegCbb.generated"))

  // ---- 单模块文档（uart 模块） ----
  private val uartAlloc = sysMap.moduleByName("uart")
  private val uartMap = uartAlloc.toRegFileMap
  write("uart_regs.json", JsonGen.generate(uartMap))
  write("uart_regs.h",   CHeaderGen.generate(uartMap))
  write("uart_regs.md",  MarkdownGen.generate(uartMap))
  write("uart_regs.html", HtmlGen.generate(uartMap))

  // ---- RTL（需要 firtool / CIRCT） ----
  write("UartDemo.sv", circt.stage.ChiselStage.emitSystemVerilog(new UartDemo))
  write("UartAxiDemo.sv", circt.stage.ChiselStage.emitSystemVerilog(new UartAxiDemo))
  write("UartSystemDemo.sv", circt.stage.ChiselStage.emitSystemVerilog(new UartSystemDemo))
  write("UartSystemAxiDemo.sv", circt.stage.ChiselStage.emitSystemVerilog(new UartSystemAxiDemo))

  println("Done -> " + dir.getPath)
}
