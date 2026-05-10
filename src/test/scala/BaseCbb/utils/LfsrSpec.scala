package BaseCbb.utils

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LfsrSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "LFSR-16" should "initialize to non-zero from seed" in {
    test(new Lfsr(16)) { c =>
      c.io.seed.poke("hABCD".U)
      c.io.load.poke(true.B)
      c.io.en.poke(true.B)
      c.clock.step(1)
      c.io.out.expect("hABCD".U)
    }
  }

  "LFSR-16" should "change output on each enable" in {
    test(new Lfsr(16)) { c =>
      c.io.seed.poke(1.U)
      c.io.load.poke(true.B)
      c.io.en.poke(true.B)
      c.clock.step(1)
      val v1 = c.io.out.peek().litValue

      c.io.load.poke(false.B)
      c.clock.step(1)
      val v2 = c.io.out.peek().litValue

      v2 should not be v1
    }
  }

  "LFSR-16" should "hold value when enable is low" in {
    test(new Lfsr(16)) { c =>
      c.io.seed.poke("h1234".U)
      c.io.load.poke(true.B)
      c.io.en.poke(true.B)
      c.clock.step(1)
      val v1 = c.io.out.peek().litValue

      c.io.load.poke(false.B)
      c.io.en.poke(false.B)
      c.clock.step(1)
      val v2 = c.io.out.peek().litValue

      v2 shouldBe v1
    }
  }

  "LFSR-8" should "work with 8-bit width" in {
    test(new Lfsr(8)) { c =>
      c.io.seed.poke(0x55.U)
      c.io.load.poke(true.B)
      c.io.en.poke(true.B)
      c.clock.step(1)
      c.io.out.expect(0x55.U)

      c.io.load.poke(false.B)
      c.clock.step(1)
      // Should have changed from seed
      c.io.out.peek().litValue should not be 0x55
    }
  }

  "LFSR-32" should "work with 32-bit width" in {
    test(new Lfsr(32)) { c =>
      c.io.seed.poke("hDEADBEEF".U)
      c.io.load.poke(true.B)
      c.io.en.poke(true.B)
      c.clock.step(1)
      c.io.out.expect("hDEADBEEF".U)

      c.io.load.poke(false.B)
      c.clock.step(1)
      // Should have changed from seed
      c.io.out.peek().litValue should not be BigInt("DEADBEEF", 16)
    }
  }

  "LFSR-16" should "reload on load pulse" in {
    test(new Lfsr(16)) { c =>
      c.io.seed.poke(0xAAAA.U)
      c.io.load.poke(true.B)
      c.io.en.poke(true.B)
      c.clock.step(1)
      c.io.out.expect(0xAAAA.U)

      c.io.load.poke(false.B)
      c.clock.step(3) // Advance a few steps

      // Reload same seed
      c.io.seed.poke(0xAAAA.U)
      c.io.load.poke(true.B)
      c.clock.step(1)
      c.io.out.expect(0xAAAA.U)
    }
  }
}
