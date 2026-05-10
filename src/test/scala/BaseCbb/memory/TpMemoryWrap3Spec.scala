package BaseCbb.memory

import chisel3._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import BaseCbb.memory.Memory
import BaseCbb.memory.MemoryProtectType

/** TpMemoryWrap3 test cases */
class TpMemoryWrap3Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // Test basic write/read with no ECC/Parity protection
  "TpMemoryWrap3 with no protection" should "write and read back data correctly" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMem",
        dataType = UInt(64.W),
        depth   = 128,
        protect = MemoryProtectType.ProtNone,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.io.lgc.waddr.poke(0.U)
      c.io.lgc.raddr.poke(0.U)
      c.io.lgc.wdata.poke(0.U)

      c.clock.step(2)
      c.io.dfx.initDone.expect(false.B)

      // Write value at address 5
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(5.U)
      c.io.lgc.wdata.poke(0xABCL.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)

      // Read it back
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(5.U)
      c.clock.step(5)

      c.io.lgc.rdata.expect(0xABCL.U)
      c.io.dfx.eccErr.expect(false.B)
      c.io.dfx.eccUerr.expect(false.B)
    }
  }

  // Test Parity protection: correct write/read
  "TpMemoryWrap3 with Parity" should "write and read correct data" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemParity",
        dataType = UInt(64.W),
        depth   = 128,
        protect = MemoryProtectType.Parity,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(1)

      // Write
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(10.U)
      c.io.lgc.wdata.poke(0x123456789ABCDEFL.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)
      c.clock.step(1)

      // Read
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(10.U)
      c.clock.step(5)

      c.io.lgc.rdata.expect(0x123456789ABCDEFL.U)
      c.io.dfx.eccErr.expect(false.B)
      c.io.dfx.eccUerr.expect(false.B)
    }
  }

  // Test ECC protection: correct write/read
  "TpMemoryWrap3 with ECC" should "write and read correct data" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemEcc",
        dataType = UInt(64.W),
        depth   = 256,
        protect = MemoryProtectType.ECC,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(1)

      // Write
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(7.U)
      c.io.lgc.wdata.poke(0xDEADBEEF1234L.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)
      c.clock.step(1)

      // Read
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(7.U)
      c.clock.step(5)

      c.io.lgc.rdata.expect(0xDEADBEEF1234L.U)
      c.io.dfx.eccErr.expect(false.B)
      c.io.dfx.eccUerr.expect(false.B)
    }
  }

  // Test memory initialization FSM
  "TpMemoryWrap3" should "complete memory initialization" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemInit",
        dataType = UInt(32.W),
        depth   = 64,
        protect = MemoryProtectType.ProtNone,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.io.dfx.initDone.expect(false.B)
      c.clock.step(1)

      // Start init
      c.io.dfx.init.poke(true.B)
      c.clock.step(1)
      c.io.dfx.init.poke(false.B)
      c.clock.step(1)

      // FSM in init state, initDone still false
      c.io.dfx.initDone.expect(false.B)

      // Let it run through all addresses
      c.clock.step(64)

      // initDone should now be true
      c.io.dfx.initDone.expect(true.B)
    }
  }

  // Test CheckIn flopping
  "TpMemoryWrap3 with CheckIn" should "flop write signals together" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemCheckIn",
        dataType = UInt(32.W),
        depth   = 128,
        protect = MemoryProtectType.ProtNone,
        flopIn  = false,
        flopOut = false,
        CheckIn = true
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)

      // Write with CheckIn enabled: signals captured on we=1
      // and appear at memory one cycle later
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(8.U)
      c.io.lgc.wdata.poke(0xABCD1234L.U)
      c.clock.step(1)

      // On next cycle, write is visible at memory
      c.io.lgc.we.poke(false.B)
      c.io.lgc.waddr.poke(0.U)
      c.io.lgc.wdata.poke(0.U)
      c.clock.step(1)

      // Read back
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(8.U)
      c.clock.step(5)

      c.io.lgc.rdata.expect(0xABCD1234L.U)
    }
  }

  // Test eccErrAddr is 0 when no error
  "TpMemoryWrap3" should "set eccErrAddr to 0 when no error" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemErrAddr",
        dataType = UInt(32.W),
        depth   = 128,
        protect = MemoryProtectType.ECC,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(1)

      // Write at address 15
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(15.U)
      c.io.lgc.wdata.poke(0xABCD5678L.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)
      c.clock.step(1)

      // Read from address 15
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(15.U)
      c.clock.step(5)

      c.io.lgc.rdata.expect(0xABCD5678L.U)
      c.io.dfx.eccErrAddr.expect(0.U) // no error, addr should be 0
    }
  }

  // Test memory initialization clears all locations
  "TpMemoryWrap3" should "clear all memory locations after init" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemInitClear",
        dataType = UInt(32.W),
        depth   = 16,
        protect = MemoryProtectType.ProtNone,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(1)

      // Write values at all addresses
      for (i <- 0 until 16) {
        c.io.lgc.we.poke(true.B)
        c.io.lgc.waddr.poke(i.U)
        c.io.lgc.wdata.poke((i * 0x11111111L).U)
        c.clock.step(1)
        c.io.lgc.we.poke(false.B)
        c.clock.step(1)
      }

      // Start init
      c.io.dfx.init.poke(true.B)
      c.clock.step(1)
      c.io.dfx.init.poke(false.B)
      c.clock.step(16)

      c.io.dfx.initDone.expect(true.B)

      // Read back all locations — should be 0
      for (i <- 0 until 16) {
        c.io.lgc.re.poke(true.B)
        c.io.lgc.raddr.poke(i.U)
        c.clock.step(5)
        c.io.lgc.rdata.expect(0.U)
        c.io.lgc.re.poke(false.B)
        c.clock.step(2)
      }
    }
  }

  // Test multiple back-to-back reads at different addresses
  "TpMemoryWrap3 with Parity" should "read multiple addresses correctly" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemMultiRead",
        dataType = UInt(64.W),
        depth   = 256,
        protect = MemoryProtectType.Parity,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(1)

      // Write values at multiple addresses
      val addrs = List(1.U, 5.U, 10.U, 50.U, 100.U, 200.U)
      val datas = List(0x123456789ABCL.U, 0xFEDCBA987654L.U, 0xAAAAAAAAAAAAABAL.U, 0x5555555555555555L.U, 0xDEADBEEF1234L.U, 0x1234DEADBEEFL.U)

      for ((addr, data) <- addrs.zip(datas)) {
        c.io.lgc.we.poke(true.B)
        c.io.lgc.waddr.poke(addr)
        c.io.lgc.wdata.poke(data)
        c.clock.step(1)
        c.io.lgc.we.poke(false.B)
        c.clock.step(1)
      }

      // Allow pipeline to settle after writes
      c.clock.step(4)

      // Read back each address
      for ((addr, data) <- addrs.zip(datas)) {
        c.io.lgc.re.poke(true.B)
        c.io.lgc.raddr.poke(addr)
        c.clock.step(6)
        c.io.lgc.rdata.expect(data)
        c.io.lgc.re.poke(false.B)
        c.clock.step(3)
      }
    }
  }

  // Test that DFX reports no ECC error on correct reads
  "TpMemoryWrap3 with ECC" should "report no ECC error on correct read" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemEccUerr",
        dataType = UInt(16.W),
        depth   = 64,
        protect = MemoryProtectType.ECC,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(1)

      // Write value
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(3.U)
      c.io.lgc.wdata.poke(0x1234L.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)
      c.clock.step(1)

      // Read
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(3.U)
      c.clock.step(5)

      c.io.lgc.rdata.expect(0x1234L.U)
      c.io.dfx.eccErr.expect(false.B)
      c.io.dfx.eccUerr.expect(false.B)
    }
  }

  // Test correctable error injection with ECC
  "TpMemoryWrap3 with ECC" should "inject correctable error and report eccErr" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemInjCorr",
        dataType = UInt(32.W),
        depth   = 64,
        protect = MemoryProtectType.ECC,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.dfx.injCorrEn.poke(false.B)
      c.io.dfx.injUerrEn.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(1)

      // Write value at address 0
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(0.U)
      c.io.lgc.wdata.poke(0xDEADBEEFL.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)
      c.clock.step(1)

      // Read with correctable error injection
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(0.U)
      c.io.dfx.injCorrEn.poke(true.B)
      c.io.dfx.injDone.expect(true.B)
      c.clock.step(1)
      c.io.dfx.injCorrEn.poke(false.B)
      c.clock.step(1)

      c.io.lgc.rdata.expect(0xDEADBEEFL.U)
      c.io.dfx.eccErr.expect(true.B)
      c.io.dfx.eccUerr.expect(false.B)
      c.io.lgc.uecErr.expect(false.B)

      c.io.lgc.re.poke(false.B)
      c.clock.step(2)
    }
  }

  // Test uncorrectable error injection with ECC
  "TpMemoryWrap3 with ECC" should "inject uncorrectable error and report uecErr" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemInjUerr",
        dataType = UInt(32.W),
        depth   = 64,
        protect = MemoryProtectType.ECC,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.dfx.injCorrEn.poke(false.B)
      c.io.dfx.injUerrEn.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(1)

      // Write value at address 0
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(0.U)
      c.io.lgc.wdata.poke(0xCAFEBABEL.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)
      c.clock.step(1)

      // Read with uncorrectable error injection
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(0.U)
      c.io.dfx.injUerrEn.poke(true.B)
      c.io.dfx.injDone.expect(true.B)
      c.clock.step(1)
      c.io.dfx.injUerrEn.poke(false.B)
      c.clock.step(1)

      c.io.dfx.eccUerr.expect(true.B)
      c.io.lgc.uecErr.expect(true.B)

      c.io.lgc.re.poke(false.B)
      c.clock.step(2)
    }
  }

  // Test independent read/write ports — write to one address, read another
  "TpMemoryWrap3" should "support independent read and write ports" in {
    test(new TpMemoryWrap3(
      Memory(
        name    = "TestMemDualPort",
        dataType = UInt(32.W),
        depth   = 64,
        protect = MemoryProtectType.ProtNone,
        flopIn  = false,
        flopOut = false
      )
    )).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(1)

      // Write 0xAAAA at addr 0
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(0.U)
      c.io.lgc.wdata.poke(0xAAAA.U)
      c.clock.step(1)

      // Simultaneously: write 0xBBBB at addr 1 + read addr 0
      c.io.lgc.we.poke(true.B)
      c.io.lgc.waddr.poke(1.U)
      c.io.lgc.wdata.poke(0xBBBB.U)
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(0.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)

      // Simultaneously: read addr 1
      c.io.lgc.re.poke(true.B)
      c.io.lgc.raddr.poke(1.U)
      c.clock.step(5)

      c.io.lgc.rdata.expect(0xBBBB.U)
    }
  }
}
