package BaseCbb.memory

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import BaseCbb.memory.Memory
import BaseCbb.memory.MemoryAccessType

class MemorySpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "Memory case class" should "have bypassOnConflict default false" in {
    val mem = Memory(
      name     = "test",
      dataType = UInt(32.W),
      depth    = 64
    )
    mem.bypassOnConflict shouldBe false
  }

  "Memory case class" should "accept bypassOnConflict = true" in {
    val mem = Memory(
      name             = "test",
      dataType         = UInt(32.W),
      depth            = 64,
      bypassOnConflict = true
    )
    mem.bypassOnConflict shouldBe true
  }

  "Memory case class" should "compute latency correctly" in {
    val mem = Memory(
      name     = "test",
      dataType = UInt(32.W),
      depth    = 32,
      flopIn   = true,
      flopOut  = true
    )
    mem.latency shouldBe 3 // 1 (intrinsic) + 1 (flopIn) + 1 (flopOut)
  }

  "Memory case class" should "have zero flop latency with no flops" in {
    val mem = Memory(
      name     = "test",
      dataType = UInt(32.W),
      depth    = 32,
      flopIn   = false,
      flopOut  = false
    )
    mem.latency shouldBe 1
  }

  "SpMemoryWrap3" should "compile with bypassOnConflict" in {
    val mem = Memory(
      name             = "TestMem",
      dataType         = UInt(32.W),
      depth            = 16,
      bypassOnConflict = true
    )
    assertCompiles("new SpMemoryWrap3(mem)")
  }

  "TpMemoryWrap3" should "compile with bypassOnConflict" in {
    val mem = Memory(
      name             = "TestMem",
      dataType         = UInt(32.W),
      depth            = 16,
      memoryType       = MemoryAccessType.TP,
      bypassOnConflict = true
    )
    assertCompiles("new TpMemoryWrap3(mem)")
  }

  "SpMemoryWrap3" should "compile without bypassOnConflict" in {
    val mem = Memory(
      name     = "TestMem",
      dataType = UInt(32.W),
      depth    = 16
    )
    assertCompiles("new SpMemoryWrap3(mem)")
  }

  "TpMemoryWrap3" should "compile without bypassOnConflict" in {
    val mem = Memory(
      name       = "TestMem",
      dataType   = UInt(32.W),
      depth      = 16,
      memoryType = MemoryAccessType.TP
    )
    assertCompiles("new TpMemoryWrap3(mem)")
  }
}
