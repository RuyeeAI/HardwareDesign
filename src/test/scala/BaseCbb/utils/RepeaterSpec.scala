package BaseCbb.utils
import BaseCbb.utils.annotation._
import BaseCbb.utils.cdc._
import BaseCbb.utils.queue._
import BaseCbb.utils.math._
import BaseCbb.utils.check._
import BaseCbb.utils.io._
import BaseCbb.utils.data._
import BaseCbb.utils.misc._
import BaseCbb.utils.timer._

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RepeaterSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "Repeater" should "pass through when not repeating" in {
    test(new Repeater(UInt(8.W))) { c =>
      c.io.repeat.poke(false.B)
      c.io.enq.valid.poke(true.B)
      c.io.enq.bits.poke(42.U)
      c.io.deq.ready.poke(true.B)

      c.clock.step(1)
      c.io.deq.valid.expect(true.B)
      c.io.deq.bits.expect(42.U)
    }
  }

  "Repeater" should "save and repeat when repeat is high" in {
    test(new Repeater(UInt(8.W))) { c =>
      // First cycle: write 99, repeat=false, should pass through
      c.io.repeat.poke(false.B)
      c.io.enq.valid.poke(true.B)
      c.io.enq.bits.poke(99.U)
      c.io.deq.ready.poke(true.B)

      c.clock.step(1)
      c.io.deq.valid.expect(true.B)
      c.io.deq.bits.expect(99.U)

      // Second cycle: set repeat=true and write new data 100
      // saved becomes 100, but repeat=true means we output SAVED value (99 from first cycle)
      c.io.repeat.poke(true.B)
      c.io.enq.valid.poke(true.B)
      c.io.enq.bits.poke(100.U)
      c.io.deq.ready.poke(true.B)

      c.clock.step(1)
      // With repeat=true, we output saved (which is 100 from previous input)
      // The saved value is latched when repeat && fire occurs
      c.io.deq.bits.expect(100.U)

      // Third cycle: deassert repeat - full stays true since repeat is still set
      c.io.repeat.poke(false.B)
      c.io.deq.ready.poke(true.B)
      c.clock.step(1)
      // With repeat=false, full stays true and we still output saved
      c.io.deq.bits.expect(100.U)
    }
  }

  "Repeater" should "signal full when saved" in {
    test(new Repeater(UInt(8.W))) { c =>
      c.io.repeat.poke(false.B)
      c.io.enq.valid.poke(true.B)
      c.io.enq.bits.poke(77.U)
      c.io.deq.ready.poke(true.B)

      c.clock.step(1)

      // Set repeat and fire to save
      c.io.repeat.poke(true.B)
      c.io.enq.valid.poke(true.B)
      c.io.enq.bits.poke(88.U)
      c.io.deq.ready.poke(true.B)

      c.clock.step(1)
      c.io.full.expect(true.B)
    }
  }
}