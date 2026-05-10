package BaseCbb.fifo

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SyncFifosSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "DualSPRamFifo" should "compile with default parameters" in {
    assertCompiles("new DualSPRamFifo()")
  }

  "DualSPRamFifo" should "compile with 16-bit data" in {
    assertCompiles("new DualSPRamFifo(16, 3)")
  }

  "DualSPRamFifo" should "compile with 64-bit data" in {
    assertCompiles("new DualSPRamFifo(64, 5)")
  }

  "DualSPRamFifo" should "compile with small depth" in {
    assertCompiles("new DualSPRamFifo(32, 1)")
  }

  "DualSPRamFifo" should "compile with large depth" in {
    assertCompiles("new DualSPRamFifo(32, 8)")
  }
}
