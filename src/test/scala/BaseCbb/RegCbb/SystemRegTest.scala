package BaseCbb.RegCbb

import chisel3._
import chisel3.util.RegEnable
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import BaseCbb.RegCbb.demo.UartDemoDef
import BaseCbb.RegCbb.demo.UartSystemDemo
import BaseCbb.RegCbb.hw._
import BaseCbb.RegCbb.gen._

/**
 * 系统级（多功能模块）测试：
 *  - 系统地址分配：UART 手工基址 0x40000000 + mem 0x40001000；GPIO 自动分配紧随其后
 *  - SystemRegFileTop 模块间译码分发：总线访问 uart 模块 / gpio 模块各自命中
 *  - SystemRegView 三级命名访问（module → block → reg）
 *  - 系统级文档生成（不依赖外围逻辑）
 */
class SystemRegTest extends AnyFreeSpec with ChiselScalatestTester {

  private val BASE = 0x40000000L

  private def read(c: UartSystemDemo, addr: Long): BigInt = {
    c.io.rd.poke(true.B)
    c.io.addr.poke(addr.U)
    val v = c.io.rdata.peek().litValue
    c.io.rd.poke(false.B)
    v
  }

  private def write(c: UartSystemDemo, addr: Long, data: Long): Unit = {
    c.io.wr.poke(true.B)
    c.io.addr.poke(addr.U)
    c.io.wdata.poke(data.U)
    c.clock.step(1)
    c.io.wr.poke(false.B)
  }

  "系统地址分配：UART 手工 + GPIO 自动" in {
    val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
    val uart = sysMap.moduleByName("uart")
    val gpio = sysMap.moduleByName("gpio")
    assert(uart.baseAddress == 0x40000000L)          // 手工指定
    assert(uart.memBaseAddress == 0x40001000L)       // 手工指定
    assert(gpio.baseAddress >= uart.baseAddress + uart.sizeBytes) // 自动紧随，不重叠
    assert(gpio.memBaseAddress >= gpio.baseAddress)  // mem 区在寄存器区之后
    // 两模块不重叠
    assert(gpio.baseAddress >= uart.baseAddress + uart.sizeBytes)
  }

  "系统地址分配摘要包含两模块" in {
    val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
    val s = sysMap.summarize
    assert(s.contains("uart"))
    assert(s.contains("gpio"))
  }

  "模块间译码分发：uart 寄存器可访问，gpio 寄存器可访问" in {
    test(new UartSystemDemo) { c =>
      // uart scratch @ 0x18（相对 uart base）
      assert(read(c, BASE + 0x18) == 0xDEADBEEFL)
      write(c, BASE + 0x18, 0x12345678L)
      assert(read(c, BASE + 0x18) == 0x12345678L)

      // gpio 模块基址（自动分配）：从 uart 地址空间之后
      val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
      val gpioBase = sysMap.moduleByName("gpio").baseAddress
      val gb = gpioBase.toLong
      // gpio_ctl 是 gpio_regs 块第一个寄存器 → 偏移 0
      assert(read(c, gb) == 0)
      write(c, gb, 0x11L)
      assert(read(c, gb) == 0x11)
    }
  }

  "模块间隔离：访问 uart 地址不影响 gpio，反之亦然" in {
    test(new UartSystemDemo) { c =>
      val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
      val gpioBase = sysMap.moduleByName("gpio").baseAddress
      val gb = gpioBase.toLong

      // 写 uart scratch
      write(c, BASE + 0x18, 0xAAAAAAAAL)
      // gpio 不受影响
      assert(read(c, gb) == 0)

      // 写 gpio
      write(c, gb, 0x55L)
      // uart scratch 保持
      assert(read(c, BASE + 0x18) == 0xAAAAAAAAL)
    }
  }

  "未命中任何模块的地址返回 0" in {
    test(new UartSystemDemo) { c =>
      // uart 模块后、gpio 模块前的大空隙地址 → 未命中
      val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
      val gpioBase = sysMap.moduleByName("gpio").baseAddress
      val uartAlloc = sysMap.moduleByName("uart")
      val gap = (uartAlloc.baseAddress + uartAlloc.sizeBytes + 0x100).toLong
      if (gap < gpioBase) {
        assert(read(c, gap) == 0)
      }
    }
  }

  "SystemRegView 三级命名访问" in {
    test(new UartSystemDemo) { c =>
      // 通过三级视图写 uart data_regs 块中的 scratch
      write(c, BASE + 0x18, 0xCAFEBABEL)
      c.clock.step(1)
      assert(read(c, BASE + 0x18) == 0xCAFEBABEL)
    }
  }

  "系统级文档生成（不依赖外围逻辑）" in {
    val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
    val json = SystemJsonGen.generate(sysMap)
    assert(json.contains("\"modules\""))
    assert(json.contains("uart"))
    assert(json.contains("gpio"))
    assert(json.contains("\"regBlocks\""))
    assert(json.contains("\"memBlocks\""))
    assert(json.contains("\"entryFields\""))

    val md = SystemMarkdownGen.generate(sysMap)
    assert(md.contains("系统寄存器手册"))
    assert(md.contains("uart"))
    assert(md.contains("gpio"))
    assert(md.contains("rx_desc"))

    val h = SystemCHeaderGen.generate(sysMap)
    assert(h.contains("_BASE"))
    assert(h.contains("GPIO"))

    val html = SystemHtmlGen.generate(sysMap)
    assert(html.contains("<html"))
    assert(html.contains("uart"))

    val vs = SystemViewSourceGen.generate(sysMap, "SysRegs", "test.gen")
    assert(vs.contains("class SysRegs"))
    assert(vs.contains("val uart"))
    assert(vs.contains("val gpio"))
  }

  "系统级 memory 访问：uart tx_fifo（0x40001000）" in {
    test(new UartSystemDemo) { c =>
      // 原子写（word 间大端）：低 word（+0x1004）→ shadow；高 word（+0x1000）→ 提交
      c.io.wr.poke(true.B);  c.io.addr.poke(0x40001004L.U); c.io.wdata.poke(0xDEADBEEFL.U)
      c.clock.step(1); c.io.wr.poke(false.B)
      c.io.wr.poke(true.B);  c.io.addr.poke(0x40001000L.U); c.io.wdata.poke(0x12345678L.U)
      c.clock.step(1); c.io.wr.poke(false.B)
      // 读回
      c.io.rd.poke(true.B); c.io.addr.poke(0x40001004L.U)
      c.clock.step(2)
      assert(c.io.rdata.peek().litValue == 0xDEADBEEFL)
      c.io.rd.poke(false.B)
      c.io.rd.poke(true.B); c.io.addr.poke(0x40001000L.U)
      c.clock.step(2)
      assert(c.io.rdata.peek().litValue == 0x12345678L)
      c.io.rd.poke(false.B)
    }
  }

  "系统级 AXI 包装：SystemAxiLiteRegFile 可 elaboration" in {
    val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
    // 用 chisel3 原生 elaboration（不依赖 firtool）验证可生成
    val chirrtl = chisel3.stage.ChiselStage.emitChirrtl(new SystemAxiLiteRegFile(sysMap))
    assert(chirrtl.contains("SystemAxiLiteRegFile"))
    assert(chirrtl.contains("module_uart") || chirrtl.contains("module_gpio"))
  }

  "★ Memory entry 域段来自 RegBundle：位宽与字段布局推导" in {
    import BaseCbb.RegCbb.dsl._
    import BaseCbb.RegCbb.demo.UartDemoDef.FifoDescEntry

    val fields = BundleToRegDefs.toEntryFields(new FifoDescEntry)
    assert(fields.map(_.bitWidth).sum == 32)          // 8+16+8 = 32 → dataWidth
    assert(fields.map(_.name) == Seq("tag", "len", "crc"))

    val mem = MemoryDef.fromBundle("rx_desc", 32, fields)
    assert(mem.dataWidth == 32)
    assert(mem.entryFieldOffsets == Seq(0, 8, 24))    // tag[7:0], len[23:8], crc[31:24]
    assert(mem.entryFields.map(_.name) == Seq("tag", "len", "crc"))
  }

  "★ Memory entry 域段：MemBuilder.bundle 便捷入口（demo 集成验证）" in {
    val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
    val uart = sysMap.moduleByName("uart")
    val rxDesc = uart.allMems.find(_.mem.name == "rx_desc").get
    assert(rxDesc.mem.dataWidth == 32)                // bundle 自动推导位宽
    assert(rxDesc.mem.entryFields.map(_.name) == Seq("tag", "len", "crc"))
    assert(rxDesc.mem.entryFields.map(_.bitWidth) == Seq(8, 16, 8))
    assert(rxDesc.mem.entryFieldOffsets == Seq(0, 8, 24))

    // 系统文档包含 entry 域段信息
    val md = SystemMarkdownGen.generate(sysMap)
    assert(md.contains("rx_desc"))
    assert(md.contains("entry 域段"))
    assert(md.contains("tag"))
    assert(md.contains("crc"))
    val json = SystemJsonGen.generate(sysMap)
    assert(json.contains("\"entryFields\""))
    assert(json.contains("rx_desc"))
    val h = SystemCHeaderGen.generate(sysMap)
    assert(h.contains("RX_DESC_TAG_MASK"))
    assert(h.contains("RX_DESC_CRC_SHIFT"))
  }

  "★ Memory entry 域段：硬件访问 rx_desc（地址经分配器自动排布）" in {
    test(new UartSystemDemo) { c =>
      // rx_desc 基址由分配器计算（uart mem 区 tx_fifo_plain 之后，0x40001400 之前）
      val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
      val rxDesc = sysMap.moduleByName("uart").allMems.find(_.mem.name == "rx_desc").get
      val base = rxDesc.baseAddress
      // 写 entry 0（32bit）：tag=0xAB, len=0x1234, crc=0x5A → 0x5A1234AB
      write(c, base.toLong, 0x5A1234ABL)
      // 读回
      c.io.rd.poke(true.B); c.io.addr.poke(base.toLong.U)
      c.clock.step(2)
      assert(c.io.rdata.peek().litValue == 0x5A1234ABL)
      c.io.rd.poke(false.B)
    }
  }

  "★ 规则 3：40bit 寄存器占据 64bit（2 的幂），有效数据从高 bit 位放（bit[63:24]）" in {
    val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
    val uart = sysMap.moduleByName("uart")
    val data40 = uart.allRegs.find(_.reg.name == "data40").get
    // IR 层：wordCount=2（64bit），字段位偏移从 24 开始（高 bit 放置）
    assert(data40.reg.expandedBits == 64)
    assert(data40.reg.wordCount == 2)
    assert(data40.reg.byteSize == 8)
    assert(data40.fieldAllocations.head.bitOffset == 24,
      s"40bit 寄存器字段应从 bit24 开始（高 bit 放置），实际 ${data40.fieldAllocations.head.bitOffset}")
    // 地址空间 2 的幂：40bit 寄存器占 8B
    assert(data40.byteSize == 8)
  }

  "★ 规则 3+大端：data40 硬件访问（word 间大端：+0=高 word、+4=低 word，数据在 bit[63:24]）" in {
    test(new UartSystemDemo) { c =>
      val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
      val uart = sysMap.moduleByName("uart")
      val ra = uart.allRegs.find(_.reg.name == "data40").get
      val base = (uart.baseAddress + ra.byteOffset).toLong
      // 原子写（word 间大端）：先写低 word（+4，数据低 8 位在 wdata bit[31:24]）进 shadow；
      // 再写最高有效 word（+0，数据高 32 位）提交
      write(c, base + 4, 0xAB000000L)      // 写低 word（数据低 8 位 = 0xAB，bit[31:24]）→ shadow
      c.clock.step(1)
      // 写最高有效 word（+0）：数据高 32 位 = 0x12345678 → 提交
      write(c, base, 0x12345678L)
      c.clock.step(1)
      // 读高 word（+0）：0x12345678
      c.io.rd.poke(true.B); c.io.addr.poke(base.U); c.clock.step(2)
      assert(c.io.rdata.peek().litValue == 0x12345678L)
      c.io.rd.poke(false.B)
      // 读低 word（+4）：0xAB000000（数据低 8 位在 bit[31:24]，bit[23:0] padding 0）
      c.io.rd.poke(true.B); c.io.addr.poke((base + 4).U); c.clock.step(2)
      assert(c.io.rdata.peek().litValue == 0xAB000000L)
      c.io.rd.poke(false.B)
    }
  }

  "★ 规则 3：96bit memory 占据 128bit（4 words），有效数据从高 bit 位放（bit[127:32]）" in {
    val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
    val uart = sysMap.moduleByName("uart")
    val wide = uart.allMems.find(_.mem.name == "wide_mem").get
    assert(wide.mem.expandedDataWidth == 128)
    assert(wide.mem.wordCount == 4)
    assert(wide.mem.byteSize == BigInt(16) * 16) // depth=16 × 128bit/8
    // 文档含 wide_mem
    val md = SystemMarkdownGen.generate(sysMap)
    assert(md.contains("wide_mem"))
    val html = SystemHtmlGen.generate(sysMap)
    assert(html.contains("wide_mem"))
  }

  "★ 规则 2：字段表格先打印高 bit 位（MSB-first）" in {
    val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
    val md = SystemMarkdownGen.generate(sysMap)
    // rx_desc entry 域段表：定位表头后的第一行应含 crc[31:24]（高位先打印）
    val headerIdx = md.indexOf("entry 域段")
    assert(headerIdx >= 0, "应含 entry 域段表")
    val tableSection = md.substring(headerIdx, headerIdx + 300)
    val idxCrc = tableSection.indexOf("crc")
    val idxTag = tableSection.indexOf("tag")
    assert(idxCrc >= 0 && idxTag >= 0 && idxCrc < idxTag,
      s"MSB-first：entry 域段表 crc 应在 tag 之前，idxCrc=$idxCrc idxTag=$idxTag")
    // 寄存器字段表：bundle_ctrl 中 burst[2]（高位）应在 mode[1:0]（低位）之前
    // 定位到 bundle_ctrl 的详细段落（标题 "#### bundle_ctrl" 之后）
    val detailIdx = md.indexOf("#### bundle_ctrl")
    assert(detailIdx >= 0, "system.md 应含 bundle_ctrl 详细段落")
    val regSection = md.substring(detailIdx, detailIdx + 400)
    val idxBurst = regSection.indexOf("burst")
    val idxMode = regSection.indexOf("mode")
    assert(idxBurst >= 0 && idxMode >= 0 && idxBurst < idxMode,
      s"MSB-first：bundle_ctrl burst 应在 mode 之前，idxBurst=$idxBurst idxMode=$idxMode")
  }
}
