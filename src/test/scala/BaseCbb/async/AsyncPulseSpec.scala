package BaseCbb.async

import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AsyncPulseSpec extends AnyFlatSpec with Matchers {

  "AsyncPulse" should "compile" in {
    assertCompiles("new AsyncPulse")
  }

  // Note: This module uses crossing between two clock domains (clk_a and clk_b)
  // Multi-clock simulation requires Verilator
}