package BaseCbb.async

import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SyncSpec extends AnyFlatSpec with Matchers {

  "Sync" should "compile with default parameters" in {
    assertCompiles("new Sync(2, 1)")
  }

  "Sync" should "compile with different widths" in {
    assertCompiles("new Sync(3, 8)")
    assertCompiles("new Sync(4, 32)")
  }

  "SYNC_FF" should "be a BlackBox" in {
    assertCompiles("new SYNC_FF")
  }

  // Note: Sync uses BlackBox SYNC_FF which requires external Verilog implementation
  // Full multi-clock simulation requires Verilator
}