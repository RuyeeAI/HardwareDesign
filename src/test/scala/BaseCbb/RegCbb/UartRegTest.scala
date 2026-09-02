package BaseCbb.RegCbb

import chisel3._
import chisel3.util.{RegEnable, ShiftRegister}
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import BaseCbb.RegCbb.demo.UartDemo
import BaseCbb.RegCbb.demo.UartDemoDef
import BaseCbb.RegCbb.hw._

/**
 * UartDemo 总线侧冒烟测试：
 *  - RW 寄存器写/读回、复位值
 *  - RO 寄存器读回（验证 v2 修复：v1 中 RO 读回恒 0）
 *  - WO 寄存器读回 0、写触发发送
 *  - W1C 硬件置位 + 软件写 1 清除
 *  - 64bit 原子/非原子寄存器
 *  - memory 地址空间（64bit 原子访问、延迟 ack 等待、status 错误）
 */
class UartRegTest extends AnyFreeSpec with ChiselScalatestTester {

  private val BASE = 0x40000000L

  private def read(c: UartDemo, addr: Long): BigInt = {
    c.io.rd.poke(true.B)
    c.io.addr.poke(addr.U)
    val v = c.io.rdata.peek().litValue
    c.io.rd.poke(false.B)
    v
  }

  private def write(c: UartDemo, addr: Long, data: Long): Unit = {
    c.io.wr.poke(true.B)
    c.io.addr.poke(addr.U)
    c.io.wdata.poke(data.U)
    c.clock.step(1)
    c.io.wr.poke(false.B)
  }

  /**
   * memory 读（请求-响应协议）：状态机一拍建立（rd 电平），零延迟用户侧同拍 ack。
   * step(2)：若上一条访问尚未完成（状态机 busy），读请求会晚一拍建立；
   * 完成后数据经 ack 采样寄存，第二拍稳定可读。
   */
  private def readMem(c: UartDemo, addr: Long): BigInt = {
    c.io.rd.poke(true.B)
    c.io.addr.poke(addr.U)
    c.clock.step(2)
    val v = c.io.rdata.peek().litValue
    c.io.rd.poke(false.B)
    v
  }

  "RW scratch 写读回与复位值" in {
    test(new UartDemo) { c =>
      // 复位值 0xDEADBEEF
      assert(read(c, BASE + 0x18) == 0xDEADBEEFL)
      write(c, BASE + 0x18, 0x12345678L)
      assert(read(c, BASE + 0x18) == 0x12345678L)
    }
  }

  "RO 寄存器读回（v1 bug 修复验证）" in {
    test(new UartDemo) { c =>
      // rx_data_ro 由用户逻辑驱动为 0x5A
      assert(read(c, BASE + 0x0C) == 0x5A)
      // status_ro：复位后 tx_busy=0, tx_done=0
      assert(read(c, BASE + 0x04) == 0)
      // ctrl 中的 RO 字段 version=2（高 4 位）
      assert((read(c, BASE + 0x00) & 0x1E000) == (2L << 13))
    }
  }

  "WO 寄存器读回 0 且写触发发送" in {
    test(new UartDemo) { c =>
      assert(read(c, BASE + 0x08) == 0) // 写前读回 0
      write(c, BASE + 0x08, 0xAB)
      c.clock.step(1)
      assert(read(c, BASE + 0x08) == 0) // 写后仍读回 0
      // 发送进行中：status_ro.tx_busy 应为 1（baud_div 复位 4，至少忙 10*5 周期）
      c.clock.step(5)
      assert((read(c, BASE + 0x04) & 0x1) == 1)
    }
  }

  "W1C 硬件置位 + 软件写1清除" in {
    test(new UartDemo) { c =>
      assert(read(c, BASE + 0x10) == 0)
      // 触发发送（baud_div=4 → 每 5 拍 1 bit，10 bit 帧）
      write(c, BASE + 0x08, 0x55)
      c.clock.step(120)
      // 发送完成 → 硬件置位 irq_w1c.tx_done(bit0)
      assert((read(c, BASE + 0x10) & 0x1) == 1)
      // 软件写 1 清除
      write(c, BASE + 0x10, 0x1)
      assert((read(c, BASE + 0x10) & 0x1) == 0)
    }
  }

  "64bit 原子寄存器（word 间大端：低地址=高有效 word，写 +0x1C 提交）" in {
    test(new UartDemo) { c =>
      // data64 @ 0x1c（word0 = 最高有效 word = bit[63:32]）/ 0x20（word1 = 低 word = bit[31:0]）
      // word 间大端：低地址存高有效 word；原子提交 = 写最高有效 word（+0x1C）
      assert(read(c, BASE + 0x1C) == 0)
      write(c, BASE + 0x20, 0x11111111L)   // 写低 word（bit[31:0]）：进入 shadow，未提交
      assert(read(c, BASE + 0x20) == 0)    // 读回旧值
      write(c, BASE + 0x1C, 0x22222222L)   // 写最高有效 word（bit[63:32]）：一次提交 64bit
      assert(read(c, BASE + 0x20) == 0x11111111L)  // 低 word = 低 32 位
      assert(read(c, BASE + 0x1C) == 0x22222222L)  // 高 word = 高 32 位
      assert(read(c, BASE + 0x20) == 0x11111111L) // 重复读仍稳定
    }
  }

  "64bit 非原子寄存器（word 间大端：+0x24=高 word，+0x28=低 word）" in {
    test(new UartDemo) { c =>
      // data64_plain @ 0x24（word0 = 高 word = bit[63:32]）/ 0x28（word1 = 低 word = bit[31:0]）
      write(c, BASE + 0x24, 0xBBBBBBBBL)   // 写高 word（bit[63:32]）
      assert(read(c, BASE + 0x24) == 0xBBBBBBBBL) // 高 word 立即生效
      write(c, BASE + 0x28, 0xAAAAAAAAL)   // 写低 word（bit[31:0]）
      assert(read(c, BASE + 0x28) == 0xAAAAAAAAL) // 低 word 立即生效
      assert(read(c, BASE + 0x24) == 0xBBBBBBBBL) // 高 word 保持
    }
  }

  "memory 64bit 原子访问（word 间大端：+0x1000=高 word，写 +0x1000 提交）" in {
    test(new UartDemo) { c =>
      // tx_fifo @ 0x40001000（word0=高 word=bit[63:32]）/ 0x40001004（word1=低 word=bit[31:0]）
      assert(readMem(c, 0x40001000L) == 0)
      write(c, 0x40001004L, 0xDEADBEEFL)  // 写低 word（bit[31:0]）：进 shadow，SRAM 未变
      assert(readMem(c, 0x40001004L) == 0)
      write(c, 0x40001000L, 0x12345678L)  // 写最高有效 word（bit[63:32]）：一次提交 64bit 到 SRAM
      assert(readMem(c, 0x40001004L) == 0xDEADBEEFL)
      assert(readMem(c, 0x40001000L) == 0x12345678L)
    }
  }

  "memory 非原子多字写（word 间大端：读-改-写）" in {
    test(new UartDemo) { c =>
      // tx_fifo_plain @ 0x40001200（word0=高 word）/ 0x40001204（word1=低 word），非原子 → 内部 RMW
      c.io.wr.poke(true.B);  c.io.addr.poke(0x40001200L.U); c.io.wdata.poke(0xA5A5A5A5L.U)
      c.clock.step(3); c.io.wr.poke(false.B)   // RMW：读请求→ack→合并写回→ack
      assert(readMem(c, 0x40001200L) == 0xA5A5A5A5L) // 高 word 已写
      assert(readMem(c, 0x40001204L) == 0)            // 低 word 保持 0（RMW 不破坏）
      c.io.wr.poke(true.B);  c.io.addr.poke(0x40001204L.U); c.io.wdata.poke(0x5A5A5A5AL.U)
      c.clock.step(3); c.io.wr.poke(false.B)
      assert(readMem(c, 0x40001200L) == 0xA5A5A5A5L) // 高 word 保持
      assert(readMem(c, 0x40001204L) == 0x5A5A5A5AL) // 低 word 已写
    }
  }

  "RegBundle（GenBundle 风格）寄存器" in {
    test(new UartDemo) { c =>
      // bundle 寄存器地址从分配器动态获取（避免硬编码偏移随布局漂移）
      val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
      val uart = sysMap.moduleByName("uart")
      def regAddr(name: String): Long = {
        val ra = uart.allRegs.find(_.reg.name == name).get
        (uart.baseAddress + ra.byteOffset).toLong
      }
      val ctrlAddr = regAddr("bundle_ctrl")
      val statusAddr = regAddr("bundle_status_ro")
      val scratchAddr = regAddr("bundle_scratch_ro")
      // bundle_ctrl：mode[1:0] + burst[2]；写 burst=1
      write(c, ctrlAddr, 0x4)
      c.clock.step(2)
      assert((read(c, ctrlAddr) & 0x7) == 0x4)          // 读回
      assert((read(c, statusAddr) & 0x1) == 1)          // bundle_status_ro.link_up 由硬件置位
      assert(read(c, scratchAddr) == 0x7)               // bundle_scratch_ro 由硬件驱动
    }
  }
}

/**
 * 自定义用户侧逻辑的 memory 测试模块：
 *  - memAckDelay：读响应 ack 延迟拍数（0 = 零延迟，同拍返回；>0 = 等待拍数）
 *  - memStatus：读响应错误状态（非 0 = 数据无效）
 */
class MemRespDemo(memAckDelay: Int = 0, memStatus: Int = 0) extends Module {
  val io = IO(new Bundle {
    val wr    = Input(Bool())
    val rd    = Input(Bool())
    val addr  = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
  })

  private val sysMap = AddressAllocator.allocateSystem(UartDemoDef.build)
  private val uartAlloc = sysMap.moduleByName("uart")
  private val map = uartAlloc.toRegFileMap
  private val regFile = Module(new RegFileTop(map))
  RegView(map, regFile) // 未使用的用户侧输入默认 0

  regFile.io.wr := io.wr
  regFile.io.rd := io.rd
  regFile.io.addr := io.addr
  regFile.io.wdata := io.wdata
  io.rdata := regFile.io.rdata

  // 用户侧逻辑：64bit SRAM + 请求-响应（rd/wr 电平保持，ack 返回后释放）
  private val sram = Mem(64, UInt(64.W))
  private val mp = regFile.io.memPorts.elements("tx_fifo").asInstanceOf[MemPortIO]
  when(mp.wr) { sram.write(mp.waddr, mp.wdata) }
  mp.rdata := sram.read(mp.raddr) // raddr 在请求期间由状态机保持
  if (memAckDelay <= 0) {
    mp.ack := mp.rd || mp.wr // 零延迟：获得带宽即同拍响应
  } else {
    mp.ack := ShiftRegister(mp.rd || mp.wr, memAckDelay, false.B, true.B) // 延迟响应
  }
  mp.status := memStatus.U(3.W) // 0=OK；测试用 2=010 不可纠正错误

  // 其余 memory 端口挂接（测试不访问，简单 0 响应 + 立即 ack）
  Seq("tx_fifo_plain", "rx_desc").foreach { n =>
    val p = regFile.io.memPorts.elements(n).asInstanceOf[MemPortIO]
    p.rdata := 0.U(p.rdata.getWidth.W)
    p.ack := p.rd || p.wr
    p.status := MemStatus.OK
  }
  val pWide = regFile.io.memPorts.elements("wide_mem").asInstanceOf[MemPortIO]
  pWide.rdata := 0.U(pWide.rdata.getWidth.W)
  pWide.ack := pWide.rd || pWide.wr
  pWide.status := MemStatus.OK
}

class MemProtocolTest extends AnyFreeSpec with ChiselScalatestTester {

  "memory 延迟 ack 响应（等待用户侧逻辑带宽）" in {
    test(new MemRespDemo(memAckDelay = 2)) { c =>
      // 原子写（word 间大端）：低 word（+0x1004，shadow，立即）+ 高 word（+0x1000，提交，wr 等待 ack 2 拍）
      c.io.wr.poke(true.B);  c.io.addr.poke(0x40001004L.U); c.io.wdata.poke(0xDEADBEEFL.U)
      c.clock.step(1); c.io.wr.poke(false.B)
      c.io.wr.poke(true.B);  c.io.addr.poke(0x40001000L.U); c.io.wdata.poke(0x12345678L.U)
      c.clock.step(1) // t1: stWrWait，wr 电平拉高
      c.clock.step(2) // t2-t3: ack 延迟 2 拍后返回，提交完成
      c.io.wr.poke(false.B)

      // 读低 word（+0x1004）：rd 电平发出后等待 2 拍 ack
      c.io.rd.poke(true.B)
      c.io.addr.poke(0x40001004L.U)
      c.clock.step(1) // t1: stRdWait，rd 电平拉高
      c.clock.step(1) // t2: ack 尚未返回（rd 保持）
      c.clock.step(1) // t3: ack=1，数据同拍返回
      assert(c.io.rdata.peek().litValue == 0xDEADBEEFL)
      c.io.rd.poke(false.B)
      // 读高 word（+0x1000）
      c.io.rd.poke(true.B)
      c.io.addr.poke(0x40001000L.U)
      c.clock.step(1); c.clock.step(1); c.clock.step(1)
      assert(c.io.rdata.peek().litValue == 0x12345678L)
      c.io.rd.poke(false.B)
    }
  }

  "memory status 编码：非OK（010 不可纠正错误）→ 数据无效" in {
    test(new MemRespDemo(memStatus = 2)) { c =>
      // 写入（写路径不受 status 影响；word 间大端：+0x1004=低 word，+0x1000=高 word 提交）
      c.io.wr.poke(true.B);  c.io.addr.poke(0x40001004L.U); c.io.wdata.poke(0x11111111L.U)
      c.clock.step(1); c.io.wr.poke(false.B)
      c.io.wr.poke(true.B);  c.io.addr.poke(0x40001000L.U); c.io.wdata.poke(0x22222222L.U)
      c.clock.step(1); c.io.wr.poke(false.B)
      // 读（零延迟 ack）：status=010 → 数据无效 → 0
      c.io.rd.poke(true.B)
      c.io.addr.poke(0x40001000L.U)
      c.clock.step(1)
      assert(c.io.rdata.peek().litValue == 0)
      c.io.rd.poke(false.B)
    }
  }
}
