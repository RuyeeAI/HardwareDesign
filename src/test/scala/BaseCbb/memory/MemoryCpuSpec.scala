package BaseCbb.memory

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import BaseCbb.memory.Memory
import BaseCbb.memory.MemoryProtectType

/** CPU access (RsAccess) test cases for SpMemoryWrap3 and TpMemoryWrap3 */
class MemoryCpuSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ===========================================================================
  // SpMemoryWrap3 CPU access tests
  // ===========================================================================

  "SpMemoryWrap3 with RsAccess" should "CPU read when user idle returns correct data and ACK" in {
    test(new SpMemoryWrap3(
      Memory(
        name = "SpCpuRd", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 32
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B); c.io.lgc.re.poke(false.B)
      c.clock.step(2)

      // Write 0xABCD via user logic to addr 5
      c.io.lgc.we.poke(true.B); c.io.lgc.addr.poke(5.U); c.io.lgc.wdata.poke(0xABCDL.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)
      c.clock.step(2)

      // CPU read addr 5 — ACK pulses 2 cycles after re (latency=1)
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(5.U)
      c.clock.step(2)
      c.io.cpu.ack.expect(true.B)
      c.io.cpu.rdata.expect(0xABCDL.U)
      c.io.cpu.status.expect(0.U)

      // ACK auto-clears next cycle (single-cycle pulse)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.re.poke(false.B)
    }
  }

  "SpMemoryWrap3 with RsAccess" should "CPU write when user idle succeeds and returns ACK" in {
    test(new SpMemoryWrap3(
      Memory(
        name = "SpCpuWr", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 32
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B); c.io.lgc.re.poke(false.B)
      c.clock.step(2)

      // CPU write 0xBEEF to addr 3 — ACK pulses 2 cycles after we
      c.io.cpu.we.poke(true.B); c.io.cpu.addr.poke(3.U); c.io.cpu.wdata.poke(0xBEEFL.U)
      c.clock.step(2)
      c.io.cpu.ack.expect(true.B)
      c.io.cpu.status.expect(0.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.we.poke(false.B)

      // Verify via user logic read
      c.io.lgc.re.poke(true.B); c.io.lgc.addr.poke(3.U)
      c.clock.step(4)
      c.io.lgc.rdata.expect(0xBEEFL.U)
    }
  }

  "SpMemoryWrap3 with RsAccess" should "CPU read blocked by user read but NOT by user write" in {
    test(new SpMemoryWrap3(
      Memory(
        name = "SpCpuSep", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 32
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.clock.step(2)

      // Pre-fill some data
      c.io.lgc.we.poke(true.B); c.io.lgc.addr.poke(10.U); c.io.lgc.wdata.poke(0xAAAA.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)

      // User writes while CPU reads — CPU read should NOT be blocked
      c.io.lgc.we.poke(true.B); c.io.lgc.addr.poke(20.U); c.io.lgc.wdata.poke(0xBBBB.U)
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(10.U)
      c.clock.step(2)
      c.io.lgc.we.poke(false.B)

      // CPU read should complete successfully (not blocked by user write)
      c.io.cpu.ack.expect(true.B)
      c.io.cpu.rdata.expect(0xAAAA.U)
      c.io.cpu.status.expect(0.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.re.poke(false.B)
    }
  }

  "SpMemoryWrap3 with RsAccess" should "CPU read blocked by user read waits and then succeeds" in {
    test(new SpMemoryWrap3(
      Memory(
        name = "SpCpuBlk", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 128
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.clock.step(2)

      // Pre-fill data
      c.io.lgc.we.poke(true.B); c.io.lgc.addr.poke(7.U); c.io.lgc.wdata.poke(0x7777.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)

      // User reads continuously, CPU wants to read — CPU blocked by user read
      c.io.lgc.re.poke(true.B); c.io.lgc.addr.poke(8.U)
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(7.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B) // blocked

      // User stops reading → CPU gets slot immediately
      c.io.lgc.re.poke(false.B)
      c.clock.step(2) // ACK pulses 2 cycles after unblock
      c.io.cpu.ack.expect(true.B)
      c.io.cpu.rdata.expect(0x7777.U)
      c.io.cpu.status.expect(0.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.re.poke(false.B)
    }
  }

  "SpMemoryWrap3 with RsAccess" should "timeout when user never yields and return status=3" in {
    test(new SpMemoryWrap3(
      Memory(
        name = "SpCpuTO", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 8
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.clock.step(2)

      // User hogs the read port
      c.io.lgc.re.poke(true.B); c.io.lgc.addr.poke(0.U)
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(5.U)

      // Wait for timeout: 1 to enter sCpuWait + 9 for cpuWaitCnt to reach 8 = step(10)
      c.clock.step(10)

      // Timeout fires at step 10: status=3, rdata=all-1s, ACK=1 (single-cycle pulse)
      c.io.cpu.ack.expect(true.B)
      c.io.cpu.status.expect(3.U)
      c.io.cpu.rdata.expect(0xFFFFFFFFL.U)

      // ACK auto-clears next cycle
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.re.poke(false.B)
      c.io.lgc.re.poke(false.B)
    }
  }

  "SpMemoryWrap3 with RsAccess" should "assert backpressure after idleCycleTh0" in {
    test(new SpMemoryWrap3(
      Memory(
        name = "SpCpuBP", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 128
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.clock.step(2)

      // Set cpuCfg idleCycleTh0 = 4
      c.io.cpuCfg.idleCycleTh0.poke(4.U)
      // User hogs memory
      c.io.lgc.re.poke(true.B); c.io.lgc.addr.poke(0.U)
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(5.U)

      // First few cycles: no backpressure
      c.clock.step(2)
      c.io.cpuBackpress.expect(false.B)

      // After Th0 cycles: backpressure asserted
      c.clock.step(3)
      c.io.cpuBackpress.expect(true.B)

      // Cleanup
      c.io.cpu.re.poke(false.B); c.io.lgc.re.poke(false.B)
    }
  }

  "SpMemoryWrap3 with RsAccess" should "CPU read with ECC protection returns no error on clean read" in {
    test(new SpMemoryWrap3(
      Memory(
        name = "SpCpuEcc", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ECC, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 128
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.dfx.injCorrEn.poke(false.B); c.io.dfx.injUerrEn.poke(false.B)
      c.clock.step(2)

      // Write via user logic
      c.io.lgc.we.poke(true.B); c.io.lgc.addr.poke(1.U); c.io.lgc.wdata.poke(0x12345678L.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)

      // CPU read
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(1.U)
      c.clock.step(2)
      c.io.cpu.ack.expect(true.B)
      c.io.cpu.status.expect(0.U) // no error on clean read
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.re.poke(false.B)
    }
  }

  // ===========================================================================
  // TpMemoryWrap3 CPU access tests
  // ===========================================================================

  "TpMemoryWrap3 with RsAccess" should "CPU read when user idle returns correct data and ACK" in {
    test(new TpMemoryWrap3(
      Memory(
        name = "TpCpuRd", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 32
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B); c.io.lgc.re.poke(false.B)
      c.clock.step(2)

      // Write via user
      c.io.lgc.we.poke(true.B); c.io.lgc.waddr.poke(5.U); c.io.lgc.wdata.poke(0xABCDL.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)

      // CPU read — ACK pulses 2 cycles after re
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(5.U)
      c.clock.step(2)
      c.io.cpu.ack.expect(true.B)
      c.io.cpu.rdata.expect(0xABCDL.U)
      c.io.cpu.status.expect(0.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.re.poke(false.B)
    }
  }

  "TpMemoryWrap3 with RsAccess" should "CPU write when user idle succeeds and returns ACK" in {
    test(new TpMemoryWrap3(
      Memory(
        name = "TpCpuWr", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 32
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B); c.io.lgc.re.poke(false.B)
      c.clock.step(2)

      // CPU write — ACK pulses 2 cycles after we
      c.io.cpu.we.poke(true.B); c.io.cpu.addr.poke(3.U); c.io.cpu.wdata.poke(0xBEEFL.U)
      c.clock.step(2)
      c.io.cpu.ack.expect(true.B)
      c.io.cpu.status.expect(0.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.we.poke(false.B)

      // Verify via user logic read
      c.io.lgc.re.poke(true.B); c.io.lgc.raddr.poke(3.U)
      c.clock.step(4)
      c.io.lgc.rdata.expect(0xBEEFL.U)
    }
  }

  "TpMemoryWrap3 with RsAccess" should "CPU read not blocked by user write (separate ports)" in {
    test(new TpMemoryWrap3(
      Memory(
        name = "TpCpuSep1", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 128
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.clock.step(2)

      // Pre-fill data at addr 10
      c.io.lgc.we.poke(true.B); c.io.lgc.waddr.poke(10.U); c.io.lgc.wdata.poke(0xAAAA.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)

      // User writes to addr 20 while CPU reads addr 10 — no conflict
      c.io.lgc.we.poke(true.B); c.io.lgc.waddr.poke(20.U); c.io.lgc.wdata.poke(0xBBBB.U)
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(10.U)
      c.clock.step(2)
      c.io.lgc.we.poke(false.B)

      c.io.cpu.ack.expect(true.B)
      c.io.cpu.rdata.expect(0xAAAA.U)
      c.io.cpu.status.expect(0.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.re.poke(false.B)
    }
  }

  "TpMemoryWrap3 with RsAccess" should "CPU write not blocked by user read (separate ports)" in {
    test(new TpMemoryWrap3(
      Memory(
        name = "TpCpuSep2", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 128
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.clock.step(2)

      // User reads from addr 0 while CPU writes to addr 5
      c.io.lgc.re.poke(true.B); c.io.lgc.raddr.poke(0.U)
      c.io.cpu.we.poke(true.B); c.io.cpu.addr.poke(5.U); c.io.cpu.wdata.poke(0xCCCCL.U)
      c.clock.step(2)
      c.io.lgc.re.poke(false.B)

      c.io.cpu.ack.expect(true.B)
      c.io.cpu.status.expect(0.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.we.poke(false.B)

      // Verify CPU write
      c.io.lgc.re.poke(true.B); c.io.lgc.raddr.poke(5.U)
      c.clock.step(4)
      c.io.lgc.rdata.expect(0xCCCCL.U)
    }
  }

  "TpMemoryWrap3 with RsAccess" should "CPU read blocked by user read waits and then succeeds" in {
    test(new TpMemoryWrap3(
      Memory(
        name = "TpCpuBlk", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 128
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.clock.step(2)

      // Pre-fill
      c.io.lgc.we.poke(true.B); c.io.lgc.waddr.poke(7.U); c.io.lgc.wdata.poke(0x7777.U)
      c.clock.step(1); c.io.lgc.we.poke(false.B); c.clock.step(2)

      // User reads (blocking CPU read on same port)
      c.io.lgc.re.poke(true.B); c.io.lgc.raddr.poke(8.U)
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(7.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B) // blocked

      // User stops reading → CPU gets slot
      c.io.lgc.re.poke(false.B)
      c.clock.step(2) // ACK pulses 2 cycles after unblock
      c.io.cpu.ack.expect(true.B)
      c.io.cpu.rdata.expect(0x7777.U)
      c.io.cpu.status.expect(0.U)
      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.re.poke(false.B)
    }
  }

  "TpMemoryWrap3 with RsAccess" should "timeout and return status=3 when blocked too long" in {
    test(new TpMemoryWrap3(
      Memory(
        name = "TpCpuTO", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 8
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.clock.step(2)

      // User hogs read port
      c.io.lgc.re.poke(true.B); c.io.lgc.raddr.poke(0.U)
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(5.U)

      c.clock.step(10)

      c.io.cpu.ack.expect(true.B)
      c.io.cpu.status.expect(3.U)
      c.io.cpu.rdata.expect(0xFFFFFFFFL.U)

      c.clock.step(1)
      c.io.cpu.ack.expect(false.B)
      c.io.cpu.re.poke(false.B); c.io.lgc.re.poke(false.B)
    }
  }

  "TpMemoryWrap3 with RsAccess" should "assert backpressure after idleCycleTh0" in {
    test(new TpMemoryWrap3(
      Memory(
        name = "TpCpuBP", dataType = UInt(32.W), depth = 64,
        protect = MemoryProtectType.ProtNone, flopIn = false, flopOut = false,
        RsAccess = true, RsMemoryDisLat = 128
      )
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.clock.step(2)

      c.io.cpuCfg.idleCycleTh0.poke(4.U)
      c.io.lgc.re.poke(true.B); c.io.lgc.raddr.poke(0.U)
      c.io.cpu.re.poke(true.B); c.io.cpu.addr.poke(5.U)

      c.clock.step(2)
      c.io.cpuBackpress.expect(false.B)

      c.clock.step(3)
      c.io.cpuBackpress.expect(true.B)

      c.io.cpu.re.poke(false.B); c.io.lgc.re.poke(false.B)
    }
  }
}
