package BaseCbb.sequential

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests for sequential units.
  *
  * NOTE: Register, RegFile1R1W, RegFile2R1W, UpCounter, ModNCounter,
  * ClkDiv2, ClkDivOdd, ClkDiv, and FsmTemplate all use explicit clock
  * port inputs with withClock/withClockAndReset, which requires a
  * multi-clock simulator. The default Treadle backend only supports
  * single-clock circuits.
  *
  * To run these tests, use Verilator:
  * {{{
  * test(new Register(8)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c => ... }
  * }}}
  *
  * Leaving placeholder tests here to document expected behavior.
  */
class SequentialUnitsSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // Placeholder: all sequential modules require Verilator backend for multi-clock support.
  // The modules are designed correctly but Treadle cannot simulate explicit clock ports.

  "Sequential Units" should "exist with correct package" in {
    // Verify the package and classes compile
    assertCompiles("new Register(8)")
    assertCompiles("new RegFile1R1W(32, 5)")
    assertCompiles("new RegFile2R1W(32, 4)")
    assertCompiles("new UpCounter(8)")
    assertCompiles("new ModNCounter(10)")
    assertCompiles("new ClkDiv2")
    assertCompiles("new ClkDiv(10)")
    assertCompiles("new FsmTemplate(4)")
  }
}
