package BaseCbb.sequential

import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClockDividerSpec extends AnyFlatSpec with Matchers {

  "ClockDivider2" should "be a BlackBox" in {
    assertCompiles("new ClockDivider2")
  }

  "ClockDivider3" should "be a BlackBox" in {
    assertCompiles("new ClockDivider3")
  }

  "Pow2ClockDivider" should "compile" in {
    assertCompiles("new Pow2ClockDivider(2)")
  }

  "Pow2ClockDivider" should "compile with different powers" in {
    assertCompiles("new Pow2ClockDivider(1)")
    assertCompiles("new Pow2ClockDivider(4)")
  }

  // Note: ClockDivider2/3 are BlackBoxes requiring external Verilog implementations
  // Pow2ClockDivider uses these BlackBoxes internally
}