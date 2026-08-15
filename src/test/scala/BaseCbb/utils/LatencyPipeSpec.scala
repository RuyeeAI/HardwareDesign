package BaseCbb.utils
import BaseCbb.misc.{LatencyPipe, LatencyPipeV, RegEn}
import BaseCbb.utils.timer._
import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LatencyPipeSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "LatencyPipe" should "delay data by configured latency" in {
    test(new LatencyPipe(UInt(8.W), 3)) { c =>
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(42.U)

      c.clock.step(4)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(42.U)
    }
  }

  "LatencyPipe" should "pass through with latency=0" in {
    test(new LatencyPipe(UInt(8.W), 0)) { c =>
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(99.U)

      c.clock.step(1)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(99.U)
    }
  }

  "LatencyPipeV" should "delay valid data" in {
    test(new LatencyPipeV(UInt(16.W), 2)) { c =>
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(123.U)

      c.clock.step(3)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(123.U)
    }
  }

  "RegEn" should "hold value when invalid" in {
    test(new RegEn(UInt(8.W))) { c =>
      c.io.in.valid.poke(false.B)
      c.clock.step(1)
      c.io.out.valid.expect(false.B)
    }
  }
}