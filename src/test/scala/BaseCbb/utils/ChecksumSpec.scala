package BaseCbb.utils

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChecksumSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "Checksum" should "compute RFC 1071 zero checksum" in {
    test(new Checksum(16)) { c =>
      // Single zero word: sum=0, ~sum=0xFFFF
      c.io.data.poke(0.U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.last.poke(true.B)
      c.clock.step(1)
      c.io.result.expect("hFFFF".U)
    }
  }

  "Checksum" should "compute single word checksum" in {
    test(new Checksum(16)) { c =>
      c.io.data.poke("h1234".U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.last.poke(true.B)
      c.clock.step(1)
      // sum = 0x1234, ~sum = 0xEDCB
      c.io.result.expect("hEDCB".U)
    }
  }

  "Checksum" should "accumulate multiple words" in {
    test(new Checksum(16)) { c =>
      // Word 1: 0x1234
      c.io.data.poke("h1234".U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.last.poke(false.B)
      c.clock.step(1)
      c.io.sum.expect("h1234".U)

      // Word 2: 0x5678
      c.io.data.poke("h5678".U)
      c.io.first.poke(false.B)
      c.io.last.poke(true.B)
      c.clock.step(1)
      // sum = 0x1234 + 0x5678 = 0x68AC, ~sum = 0x9753
      c.io.result.expect("h9753".U)
    }
  }

  "Checksum" should "handle carry wrap-around" in {
    test(new Checksum(16)) { c =>
      // 0xFFFF + 0x0001 = 0x10000 -> wrap: 0x0000 + 1 = 0x0001
      c.io.data.poke("hFFFF".U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.last.poke(false.B)
      c.clock.step(1)

      c.io.data.poke("h0001".U)
      c.io.first.poke(false.B)
      c.io.last.poke(true.B)
      c.clock.step(1)
      c.io.result.expect("hFFFE".U)
    }
  }

  "Checksum" should "reject input when not valid" in {
    test(new Checksum(16)) { c =>
      c.io.data.poke("hABCD".U)
      c.io.valid.poke(false.B)
      c.io.first.poke(true.B)
      c.io.last.poke(true.B)
      c.clock.step(1)
      // No change — accumulator stays at 0
      c.io.sum.expect(0.U)
    }
  }

  "Checksum" should "reset on first" in {
    test(new Checksum(16)) { c =>
      // First packet: 0x1111
      c.io.data.poke("h1111".U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.last.poke(true.B)
      c.clock.step(1)
      c.io.result.expect("hEEEE".U)

      // Second packet: first resets, 0x2222
      c.io.data.poke("h2222".U)
      c.io.first.poke(true.B)
      c.io.last.poke(true.B)
      c.clock.step(1)
      // sum = 0x2222, ~sum = 0xDDDD (not accumulated from previous)
      c.io.result.expect("hDDDD".U)
    }
  }
}
