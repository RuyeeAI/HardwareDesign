package BaseCbb.async

import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AsyncBusSpec extends AnyFlatSpec with Matchers {

  "AsyncBus" should "compile with UInt type" in {
    assertCompiles("new AsyncBus(UInt(32.W))")
  }

  "AsyncBus" should "compile with different data types" in {
    assertCompiles("new AsyncBus(chisel3.SInt(16.W))")
  }

  // Note: This module implements handshake-based clock domain crossing
  // Multi-clock simulation requires Verilator
}
// re-trigger macro expansion (AsyncBus now implemented)
