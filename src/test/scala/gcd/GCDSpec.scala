package gcd

import chisel3._
import chisel3.tester._
import org.scalatest.freespec.AnyFreeSpec

class GCDSpec extends AnyFreeSpec with ChiselScalatestTester {
  "GCD should compute correct result" in {
    test(new GCD) { c =>
      c.io.a.poke(42.U)
      c.io.b.poke(12.U)
      c.io.e.poke(true.B)
      c.clock.step(1)
      while (!c.io.v.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.io.z.expect(6.U)
    }
  }
}
