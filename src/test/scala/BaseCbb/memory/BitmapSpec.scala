package BaseCbb.memory

import BaseCbb._
import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BitmapSpec extends AnyFlatSpec with Matchers {

  "Bitmap" should "compile" in {
    assertCompiles("new Bitmap(8)")
  }

  "Bitmap" should "compile with different sizes" in {
    assertCompiles("new Bitmap(16)")
    assertCompiles("new Bitmap(32)")
    assertCompiles("new Bitmap(2)")
  }

  // Note: Bitmap uses GenModule base class and requires external memory port connections
  // Full functional testing requires integration with memory subsystem
  // [touch-2] Bitmap.scala restored (uses BitmapKernel, 1=available) — force zinc recompile of assertCompiles macro
}