package BaseCbb.RegCbb

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import BaseCbb.RegCbb.demo.UartAxiDemo
import BaseCbb.RegCbb.demo.UartDemoDef
import BaseCbb.SimReset

/** AXI4-Lite 功能仿真（2026-09-13 修复的回归锁定，此前 AXI 路径零功能测试）：
 *  1. AW/W 同拍握手写地址（修复前写错地址：用的是上一笔的 wrAddrReg）
 *  2. memory 读的 r_valid 时序（修复前 ar 后固定 1 拍置 r_valid，锁到陈旧数据）
 *  3. busy 反压下连续访问不丢事务
 */
class AxiLiteFuncTest extends AnyFreeSpec with Matchers {

  private val sysMap     = AddressAllocator.allocateSystem(UartDemoDef.build)
  private val uartMap    = sysMap.moduleByName("uart").toRegFileMap
  private val BASE       = uartMap.regBaseAddress
  private val scratchOff = uartMap.regs.find(_.reg.name == "scratch").get.byteOffset
  private val memBase    = uartMap.mems.find(_.mem.name == "tx_fifo").get.baseAddress

  /** AW/W 同拍发起写（AXI-Lite 主流时序），等 b 响应。 */
  private def axiWrite(c: UartAxiDemo, addr: BigInt, data: Long): Unit = {
    c.io.axi.b_ready.poke(true.B)
    c.io.axi.aw_valid.poke(true.B)
    c.io.axi.aw_addr.poke(addr.U)
    c.io.axi.w_valid.poke(true.B)
    c.io.axi.w_data.poke(data.U)
    var guard = 0
    while ((c.io.axi.aw_ready.peek().litValue == 0 || c.io.axi.w_ready.peek().litValue == 0) && guard < 64) {
      c.clock.step(1); guard += 1
    }
    assert(guard < 64, s"aw/w ready 等待超时 @0x${addr.toString(16)}")
    c.clock.step(1) // AW/W 同拍握手
    c.io.axi.aw_valid.poke(false.B)
    c.io.axi.w_valid.poke(false.B)
    guard = 0
    while (c.io.axi.b_valid.peek().litValue == 0 && guard < 64) {
      c.clock.step(1); guard += 1
    }
    assert(guard < 64, s"b 响应等待超时 @0x${addr.toString(16)}")
    c.clock.step(1) // b 握手
  }

  /** 发起读并等待 r_valid，返回 r_data。 */
  private def axiRead(c: UartAxiDemo, addr: BigInt): BigInt = {
    c.io.axi.r_ready.poke(true.B)
    c.io.axi.ar_valid.poke(true.B)
    c.io.axi.ar_addr.poke(addr.U)
    var guard = 0
    while (c.io.axi.ar_ready.peek().litValue == 0 && guard < 64) {
      c.clock.step(1); guard += 1
    }
    assert(guard < 64, s"ar ready 等待超时 @0x${addr.toString(16)}")
    c.clock.step(1) // ar 握手
    c.io.axi.ar_valid.poke(false.B)
    guard = 0
    while (c.io.axi.r_valid.peek().litValue == 0 && guard < 64) {
      c.clock.step(1); guard += 1
    }
    assert(guard < 64, s"r 等待超时 @0x${addr.toString(16)}")
    val v = c.io.axi.r_data.peek().litValue
    c.clock.step(1) // r 握手
    v
  }

  "AW/W 同拍写 scratch 并读回" in {
    simulate(new UartAxiDemo) { c =>
      SimReset(c)
      axiWrite(c, BASE + scratchOff, 0x12345678L)
      assert(axiRead(c, BASE + scratchOff) == 0x12345678L)
    }
  }

  "memory 64bit 原子写读回（r_valid 时序回归）" in {
    simulate(new UartAxiDemo) { c =>
      SimReset(c)
      // word 间大端：+0 = 高 32bit（写它提交），+4 = 低 32bit（先写进 shadow）
      axiWrite(c, memBase + 4, 0xCAFEBABEL)
      axiWrite(c, memBase + 0, 0xDEADBEEFL)
      assert(axiRead(c, memBase + 0) == 0xDEADBEEFL)
      assert(axiRead(c, memBase + 4) == 0xCAFEBABEL)
    }
  }

  "连续 8 个单元读写（busy 反压）不丢事务" in {
    simulate(new UartAxiDemo) { c =>
      SimReset(c)
      for (i <- 0 until 8) {
        val hi = BigInt(0x1000 + i)
        val lo = BigInt(0xA000 + i * 0x10)
        axiWrite(c, memBase + i * 8 + 4, lo.toLong)
        axiWrite(c, memBase + i * 8 + 0, hi.toLong)
        assert(axiRead(c, memBase + i * 8 + 0) == hi, s"unit $i 高 word")
        assert(axiRead(c, memBase + i * 8 + 4) == lo, s"unit $i 低 word")
      }
    }
  }
}
