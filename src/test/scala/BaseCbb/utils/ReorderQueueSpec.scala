package BaseCbb.utils
import BaseCbb.utils.timer._

import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReorderQueueSpec extends AnyFlatSpec with Matchers {

  "ReorderQueue" should "compile with CAM mode" in {
    // tagSpaceSize (16) > actualSize (8) triggers CAM mode
    assertCompiles("new ReorderQueue(UInt(8.W), 4, Some(8))")
  }

  "ReorderQueue" should "compile with direct indexing mode" in {
    // tagSpaceSize == actualSize triggers direct indexing mode
    assertCompiles("new ReorderQueue(UInt(8.W), 3, Some(8))")
  }

  "ReorderQueue" should "compile with no size hint" in {
    assertCompiles("new ReorderQueue(UInt(32.W), 5)")
  }

  // Note: Full functional testing requires controlled enq/deq sequencing
  // The module is designed for integration testing with proper timing
}