package BaseCbb.utils
import BaseCbb.utils.timer._

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShaperSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "Shaper" should "pass when enough tokens" in {
    test(new Shaper(16)) { c =>
      c.io.rate.poke(10.U)
      c.io.burstSize.poke(100.U)
      c.io.interval.poke(10.U)
      c.io.pktSize.poke(5.U)
      c.io.req.poke(true.B)
      // With rate=10, tokens accumulate to ~10 after 10 cycles, enough for pktSize=5
      c.clock.step(11)
      c.io.pass.expect(true.B)
    }
  }

  "Shaper" should "block when not enough tokens" in {
    test(new Shaper(16)) { c =>
      c.io.rate.poke(0.U)
      c.io.burstSize.poke(5.U)
      c.io.interval.poke(10.U)
      c.io.pktSize.poke(3.U)
      c.io.req.poke(true.B)
      // No tokens added, should not pass
      c.clock.step(1)
      c.io.pass.expect(false.B)
    }
  }

  "Shaper" should "consume tokens on pass" in {
    test(new Shaper(16)) { c =>
      c.io.rate.poke(50.U)
      c.io.burstSize.poke(100.U)
      c.io.interval.poke(1.U)
      c.io.pktSize.poke(30.U)
      c.io.req.poke(false.B)

      // Let tokens accumulate
      c.clock.step(3)

      // Request packet - should pass with 50 tokens >= 30 pktSize
      c.io.req.poke(true.B)
      c.clock.step(1)
      c.io.pass.expect(true.B)

      // Tokens should have decreased (50 + 50 - 30 = 70)
      c.io.tokens.expect(70.U)
    }
  }

  "Shaper" should "respect burst size limit" in {
    test(new Shaper(16)) { c =>
      c.io.rate.poke(100.U)
      c.io.burstSize.poke(10.U)
      c.io.interval.poke(1.U)
      c.io.pktSize.poke(0.U)
      c.io.req.poke(false.B)

      c.clock.step(10)
      // Tokens should be capped at burstSize (10), not 1000
      c.io.tokens.expect(10.U)
    }
  }

  "Shaper" should "not pass with zero tokens" in {
    test(new Shaper(16)) { c =>
      c.io.rate.poke(0.U)
      c.io.burstSize.poke(10.U)
      c.io.interval.poke(10.U)
      c.io.pktSize.poke(1.U)
      c.io.req.poke(true.B)

      for (_ <- 0 until 5) {
        c.clock.step(1)
        c.io.pass.expect(false.B)
      }
    }
  }
}
