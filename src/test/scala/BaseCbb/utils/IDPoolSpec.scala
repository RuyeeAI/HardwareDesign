package BaseCbb.utils

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IDPoolSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "IDPool" should "allocate and free IDs" in {
    test(new IDPool(4)) { c =>
      // Initially should be valid
      c.io.alloc.ready.poke(true.B)
      c.clock.step(1)

      // Allocate ID 0
      c.io.alloc.ready.poke(true.B)
      c.io.free.valid.poke(false.B)
      c.clock.step(1)

      // Free ID 0
      c.io.free.bits.poke(0.U)
      c.io.free.valid.poke(true.B)
      c.io.alloc.ready.poke(false.B)
      c.clock.step(1)
    }
  }

  "IDPool" should "compile with different sizes" in {
    assertCompiles("new IDPool(8)")
    assertCompiles("new IDPool(16)")
    assertCompiles("new IDPool(2)")
  }

  "IDPool" should "compile with lateValid=true" in {
    assertCompiles("new IDPool(4, lateValid=true)")
  }

  "IDPool" should "compile with revocableSelect=true" in {
    assertCompiles("new IDPool(4, revocableSelect=true)")
  }
}