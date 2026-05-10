package BaseCbb.arbiter

import BaseCbb._
import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HellaArbitersSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "HellaCountingArbiter" should "grant to valid input" in {
    test(new HellaCountingArbiter(UInt(8.W), 4, 2)) { c =>
      c.io.in(0).valid.poke(true.B)
      c.io.in(0).bits.poke(10.U)
      c.io.in(1).valid.poke(false.B)
      c.io.in(2).valid.poke(false.B)
      c.io.in(3).valid.poke(false.B)
      c.io.out.ready.poke(true.B)

      c.clock.step(1)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(10.U)
    }
  }

  "HellaCountingArbiter" should "round robin when rr=true" in {
    test(new HellaCountingArbiter(UInt(8.W), 4, 2, rr=true)) { c =>
      c.io.in(0).valid.poke(true.B)
      c.io.in(0).bits.poke(1.U)
      c.io.in(1).valid.poke(true.B)
      c.io.in(1).bits.poke(2.U)
      c.io.in(2).valid.poke(true.B)
      c.io.in(2).bits.poke(3.U)
      c.io.in(3).valid.poke(true.B)
      c.io.in(3).bits.poke(4.U)
      c.io.out.ready.poke(true.B)

      c.clock.step(1)
      c.io.out.valid.expect(true.B)
    }
  }

  "HellaCountingArbiter" should "unlock after count transactions" in {
    test(new HellaCountingArbiter(UInt(8.W), 2, 3)) { c =>
      c.io.in(0).valid.poke(true.B)
      c.io.in(0).bits.poke(10.U)
      c.io.in(1).valid.poke(false.B)
      c.io.out.ready.poke(true.B)

      c.clock.step(1)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(10.U)
    }
  }

  "HellaPeekingArbiter" should "compile" in {
    assertCompiles("new HellaPeekingArbiter(UInt(8.W), 2, (x: UInt) => x > 5.U)")
  }
}