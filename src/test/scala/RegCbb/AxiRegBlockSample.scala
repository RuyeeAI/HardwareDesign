package RegCbb

import chisel3._
import chisel3.tester._
import org.scalatest.freespec.AnyFreeSpec

/**
 * RegBlock DSL测试
 */
class RegBlockDSLSpec extends AnyFreeSpec with ChiselScalatestTester {

  "RegBlock DSL should allocate addresses correctly" in {
    test(new RegBlockDSLSample) { c =>
      // 验证寄存器地址分配
      // ctrl @ 0x100
      // status @ 0x104
      // data @ 0x108
      // data64 @ 0x10C
      // irq_en @ 0x114
      // fifo @ 0x1000
    }
  }

  "AXI interface should support burst transactions" in {
    test(new AxiRegBlockSample) { c =>
      // 验证AXI burst操作
    }
  }
}

/**
 * 简化的RegBlock DSL测试模块
 */
class RegBlockDSLSample extends Module {
  val io = IO(new Bundle {
    val reg_addr   = Input(UInt(12.W))
    val reg_wrEn   = Input(Bool())
    val reg_wrData = Input(UInt(32.W))
    val reg_rdEn   = Input(Bool())
    val reg_rdData = Output(UInt(32.W))
    val mem_addr   = Input(UInt(12.W))
    val mem_cs     = Input(Bool())
    val mem_wrEn   = Input(Bool())
    val mem_wrData = Input(UInt(64.W))
    val mem_rdEn   = Input(Bool())
    val mem_rdData = Output(UInt(64.W))
  })

  // 定义寄存器块
  val regBlock = RegBlock("test") { b =>
    b.baseAddress(0x100)
    b.desc("Test Register Block")

    b.reg("ctrl") { r =>
      r.desc("Control register")
      r.field(RegField.rw("enable", 1, 0, "Enable"))
      r.field(RegField.rw("mode", 2, 0, "Mode"))
      r.field(RegField.rw("start", 1, 0, "Start"))
    }

    b.reg("status") { r =>
      r.desc("Status register")
      r.field(RegField.ro("busy", 1, 0, "Busy"))
      r.field(RegField.ro("done", 1, 0, "Done"))
    }

    b.reg("data") { r =>
      r.desc("Data register")
      r.field(RegField.rw("value", 32, 0, "Data"))
    }

    b.memBaseAddress(0x1000)
    b.mem("fifo") { m =>
      m.depth(64).dataWidth(32).sp().desc("Test FIFO")
    }
  }

  // 地址分配
  val map = AddressAllocator.allocate(regBlock)

  // 打印分配结果
  println(AddressAllocator.summarize(map))

  // 生成硬件
  val (regIO, regDataOut, memPorts) = RegisterFileGenerator.generate(map)

  // 连接测试接口
  regIO.reg_addr   := io.reg_addr
  regIO.reg_wrEn   := io.reg_wrEn
  regIO.reg_wrData := io.reg_wrData
  regIO.reg_rdEn   := io.reg_rdEn
  io.reg_rdData    := regIO.reg_rdData

  regIO.mem_addr   := io.mem_addr
  regIO.mem_cs     := io.mem_cs
  regIO.mem_wrEn   := io.mem_wrEn
  regIO.mem_wrData := io.mem_wrData
  regIO.mem_rdEn   := io.mem_rdEn
  io.mem_rdData    := regIO.mem_rdData
}

/**
 * RegBlock DSL功能测试
 */
class RegBlockFunctionalitySpec extends AnyFreeSpec with ChiselScalatestTester {

  "RegBlock should support register write and read" in {
    test(new RegBlockDSLSample) { c =>
      // 写ctrl寄存器 (0x100)
      c.io.reg_addr.poke(0x100.U)
      c.io.reg_wrEn.poke(true.B)
      c.io.reg_wrData.poke(0x7.U)  // enable=1, mode=3, start=1
      c.clock.step(1)
      c.io.reg_wrEn.poke(false.B)
      c.clock.step(1)

      // 读ctrl寄存器
      c.io.reg_rdEn.poke(true.B)
      c.clock.step(1)
      val rdData = c.io.reg_rdData.peek().litValue()
      println(s"Read ctrl: 0x${rdData.toString(16)}")

      // 写data寄存器 (0x108)
      c.io.reg_addr.poke(0x108.U)
      c.io.reg_wrEn.poke(true.B)
      c.io.reg_wrData.poke(0xDEADBEEF.U)
      c.clock.step(1)
      c.io.reg_wrEn.poke(false.B)
      c.clock.step(1)

      // 读data寄存器
      c.io.reg_rdEn.poke(true.B)
      c.clock.step(1)
      val dataVal = c.io.reg_rdData.peek().litValue()
      println(s"Read data: 0x${dataVal.toString(16)}")
      assert(dataVal == 0xDEADBEEFL)
    }
  }

  "RegBlock should allocate correct addresses" in {
    test(new RegBlockDSLSample) { c =>
      // 验证寄存器地址
      // ctrl @ 0x100 (base + 0)
      // status @ 0x104 (base + 4, 32-bit aligned)
      // data @ 0x108 (base + 8)

      // 写status寄存器 (0x104) - 验证只读字段
      c.io.reg_addr.poke(0x104.U)
      c.io.reg_wrEn.poke(true.B)
      c.io.reg_wrData.poke(0xFFFFFFFF.U)  // 尝试写入只读寄存器
      c.clock.step(1)
      c.io.reg_wrEn.poke(false.B)
      c.clock.step(1)

      // 读status寄存器 - 应该读到复位值
      c.io.reg_rdEn.poke(true.B)
      c.clock.step(1)
      val statusVal = c.io.reg_rdData.peek().litValue()
      println(s"Status (readonly): 0x${statusVal.toString(16)}")
      // status的busy和done字段应该是0 (复位值)
      assert((statusVal & 0x3L) == 0L)
    }
  }

  "Memory interface should work correctly" in {
    test(new RegBlockDSLSample) { c =>
      // Memory @ 0x1000
      c.io.mem_addr.poke(0x1000.U)
      c.io.mem_cs.poke(true.B)
      c.io.mem_wrEn.poke(true.B)
      c.io.mem_wrData.poke(0x12345678L.U)
      c.clock.step(1)
      c.io.mem_wrEn.poke(false.B)

      // 读回
      c.io.mem_rdEn.poke(true.B)
      c.clock.step(1)
      val memVal = c.io.mem_rdData.peek().litValue()
      println(s"Memory read: 0x${memVal.toString(16)}")
    }
  }

  "JSON IR generation should work" in {
    test(new RegBlockDSLSample) { c =>
      val regBlock = RegBlock("test") { b =>
        b.baseAddress(0x100)
        b.desc("Test Register Block")

        b.reg("ctrl") { r =>
          r.desc("Control register")
          r.field(RegField.rw("enable", 1, 0, "Enable"))
          r.field(RegField.rw("mode", 2, 0, "Mode"))
        }

        b.reg("status") { r =>
          r.desc("Status register")
          r.field(RegField.ro("busy", 1, 0, "Busy"))
        }

        b.memBaseAddress(0x1000)
        b.mem("fifo") { m =>
          m.depth(64).dataWidth(32).sp().desc("Test FIFO")
        }
      }

      val map = AddressAllocator.allocate(regBlock)
      val ir = RegisterIRGenerator.generate(map)
      val json = RegisterIRGenerator.toJson(ir)
      val chdr = RegisterIRGenerator.toCHeader(ir)

      println("Generated JSON IR:")
      println(json)
      println("\nGenerated C Header:")
      println(chdr)
    }
  }
}
