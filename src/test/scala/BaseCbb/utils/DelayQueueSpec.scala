package BaseCbb.utils
import BaseCbb.misc.DelayQueue
import BaseCbb.utils.timer._

import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DelayQueueSpec extends AnyFlatSpec with Matchers {

  "DelayQueue" should "compile" in {
    assertCompiles("new DelayQueue(UInt(8.W), 8)")
  }

  "DelayQueue" should "compile with different configurations" in {
    assertCompiles("new DelayQueue(chisel3.SInt(16.W), 16)")
  }

  // Note: DelayQueue requires external timer input - testing requires multi-clock or external timer
  // The module uses Queue internally and is designed for integration testing
}