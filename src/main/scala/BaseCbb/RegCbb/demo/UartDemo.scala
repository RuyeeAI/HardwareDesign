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
 *   data64      ★ 64bit RW 原子寄存器（写低字暂存、写高字一次提交）
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

  def build: RegBlockDef = RegBlock("uart") { b =>
    b.device("UART")
      .baseAddress(0x40000000L)
      .memBaseAddress(0x40001000L)
      .desc("RegCbb 演示外设：简化 UART")

    b.reg("ctrl") { r =>
      r.desc("控制寄存器")
      r.field(RegField.rw("tx_en", 1, 0, "发送使能"))
      r.field(RegField.rw("baud_div", 12, 4, "波特率分频值（4..4095）"))
      r.field(RegField.ro("version", 4, 0, "硬件版本号"))           // 字段级 RO（混排在 RW 寄存器内）
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

    b.reg("scratch") { r =>
      r.desc("测试寄存器")
      r.field(RegField("value", 32) { f =>
        f.rw().reset(0xDEADBEEFL).desc("任意读写，用于冒烟测试")
      })
    }

    b.reg("data64") { r =>
      r.desc("64 位原子寄存器：写低字(0x1c)暂存，写高字(0x20)一次提交")
      r.atomic()
      r.field(RegField.rw("value", 64, 0, "64 位数据（原子访问）"))
    }

    b.reg("data64_plain") { r =>
      r.desc("64 位非原子寄存器：低字(0x24)/高字(0x28)逐字直接写")
      r.nonAtomic()
      r.field(RegField.rw("value", 64, 0, "64 位数据（非原子访问）"))
    }

    // ★ RegBundle（GenBundle 风格）定义的寄存器组
    b.regs(BundleToRegDefs.toRegDefs(new UartBundleRegs))

    b.mem("tx_fifo") { m =>
      m.depth(64).dataWidth(64).sp()
        .atomic()
        .desc("64 位宽发送 FIFO 存储（原子访问；地址由 AddressAllocator 自动分配，从 memBase 0x40001000 起）")
    }

    b.mem("tx_fifo_plain") { m =>
      m.depth(64).dataWidth(64).sp()
        .nonAtomic()
        .desc("64 位宽普通存储（非原子：逐字读-改-写；地址自动分配在 0x40001200）")
    }
  }
}

/**
 * UART 演示模块（简单总线版本）。
 *
 * 外围逻辑连接要点：
 *  - 写侧（SW→HW）：`when(regs("tx_data_wo").wrEn) { ... regs("tx_data_wo").field("data").wrData ... }`
 *  - 读侧（HW→SW）：`regs("status_ro").field("tx_busy").roValue := txBusy`（RO 驱动）
 *  - 中断（HW→SW）：`regs("irq_w1c").field("tx_done").hwSet := txDonePulse`（硬件置位）
 *  - 读当前值：`regs("ctrl").field("baud_div").value`
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

  private val map = AddressAllocator.allocate(UartDemoDef.build)
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
  val baudDiv     = regs("ctrl").field("baud_div").value // 读取 RW 字段当前值

  when(regs("tx_data_wo").wrEn) {              // SW 写入 → 捕获数据并启动发送
    txShift := Cat(1.U(1.W), regs("tx_data_wo").field("data").wrData, 0.U(1.W))
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

  // ---- RO 驱动（用户逻辑 → 寄存器） ----
  regs("status_ro").field("tx_busy").roValue := txBusy
  regs("status_ro").field("tx_done").roValue := RegNext(txDonePulse, false.B)
  regs("ctrl").field("version").roValue       := 2.U(4.W)
  regs("rx_data_ro").field("data").roValue    := 0x5A.U(8.W) // 演示固定值

  // ---- W1C 硬件置位 ----
  regs("irq_w1c").field("tx_done").hwSet := txDonePulse
  regs("irq_w1c").field("rx_rdy").hwSet  := false.B

  // ---- RegBundle 寄存器连接 ----
  val linkUp = RegInit(false.B)
  when(regs("bundle_ctrl").wrEn) { linkUp := regs("bundle_ctrl").field("burst").wrData(0) }
  regs("bundle_status_ro").field("link_up").roValue := linkUp
  regs("bundle_scratch_ro").field("bundle_scratch_ro").roValue := 0x7.U(8.W) // 演示：RO 由硬件驱动

  // ---- 64bit 原子寄存器：硬件直写演示（hwWrEn + hwWrData） ----
  regs("data64").hwWrEn := false.B  // 默认不写（SW 经 0x1c/0x20 原子访问）

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

  // ---- 中断输出 = 中断状态 & 使能 ----
  val irqStatus = regs("irq_w1c").value
  val irqEn     = regs("irq_en").value
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

  private val map = AddressAllocator.allocate(UartDemoDef.build)
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

  // 演示：硬件持续置位"接收就绪"中断位
  regs("irq_w1c").field("rx_rdy").hwSet := true.B
  io.tx := true.B
  io.irq := regs("irq_w1c").value(1) && regs("irq_en").value(1)

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
}

/** 一键生成所有产物：sbt "runMain BaseCbb.RegCbb_v2.demo.EmitAll [outDir]" */
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

  private val map = AddressAllocator.allocate(UartDemoDef.build)
  println(AddressAllocator.summarize(map))

  // 文档/软件视图（无需 elaboration）
  write("uart_regs.json", JsonGen.generate(map))
  write("uart_regs.h",   CHeaderGen.generate(map))
  write("uart_regs.md",  MarkdownGen.generate(map))
  write("uart_regs.html", HtmlGen.generate(map))
  write("UartRegs.scala", ViewSourceGen.generate(map, "UartRegs", "BaseCbb.RegCbb.generated"))

  // RTL（需要 firtool / CIRCT）
  write("UartDemo.sv", circt.stage.ChiselStage.emitSystemVerilog(new UartDemo))
  write("UartAxiDemo.sv", circt.stage.ChiselStage.emitSystemVerilog(new UartAxiDemo))

  println("Done -> " + dir.getPath)
}
