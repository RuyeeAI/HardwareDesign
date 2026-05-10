package BaseCbb.utils

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BroadcasterSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "Broadcaster" should "broadcast to 4 outputs in round-robin" in {
    test(new Broadcaster(UInt(8.W), 4)) { c =>
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(42.U)

      // First output gets the data
      c.io.out(0).ready.poke(true.B)
      c.io.out(1).ready.poke(false.B)
      c.io.out(2).ready.poke(false.B)
      c.io.out(3).ready.poke(false.B)

      c.clock.step(1)
      // After first fires, moves to next
      c.io.in.ready.expect(false.B)
    }
  }

  "Broadcaster" should "compile with n=1" in {
    assertCompiles("new Broadcaster(UInt(8.W), 1)")
  }

  "Broadcaster" should "compile with different types" in {
    assertCompiles("new Broadcaster(chisel3.SInt(16.W), 3)")
  }
}