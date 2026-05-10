package BaseCbb.fifo

import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AsyncFifosSpec extends AnyFlatSpec with Matchers {

  "AsyncFifo" should "compile" in {
    assertCompiles("new AsyncFifo(32, 4)")
  }

  "AsyncFifo" should "compile with different sizes" in {
    assertCompiles("new AsyncFifo(64, 5)")
    assertCompiles("new AsyncFifo(16, 3)")
  }

  "AsyncZeroLatencyFifo" should "compile" in {
    assertCompiles("new AsyncZeroLatencyFifo(32, 4)")
  }

  "AsyncZeroLatencyFifo" should "compile with different sizes" in {
    assertCompiles("new AsyncZeroLatencyFifo(8, 3)")
    assertCompiles("new AsyncZeroLatencyFifo(128, 6)")
  }

  // Note: These modules use multi-clock design and require Verilator for full simulation
}