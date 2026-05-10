package BaseCbb.async

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests for async modules: Sync2, PulseSync, EdgeDetect, AsyncRstSync, Handshake, GrayCounter
  *
  * NOTE: These modules use explicit clock ports with withClockAndReset,
  * which requires Verilator backend for multi-clock simulation.
  */
class AsyncUnitsSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "Sync2" should "compile" in {
    assertCompiles("new Sync2(2)")
  }

  "PulseSync" should "compile" in {
    assertCompiles("new PulseSync")
  }

  "EdgeDetect" should "compile" in {
    assertCompiles("new EdgeDetect")
  }

  "AsyncRstSync" should "compile" in {
    assertCompiles("new AsyncRstSync")
  }

  "Handshake" should "compile with UInt type" in {
    assertCompiles("new Handshake(UInt(32.W))")
  }

  "GrayCounter" should "compile" in {
    // GrayCounter uses explicit clock input - requires Verilator for simulation
    assertCompiles("new GrayCounter(4)")
  }
}