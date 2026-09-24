package BaseCbb.utils
import BaseCbb.utils.timer._

import chisel3._
import BaseCbb.Sim._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TimerSpec extends AnyFlatSpec with Matchers {

  "SimpleTimer" should "count down and timeout" in {
    simulate(new SimpleTimer(5)) { c =>
      c.io.start.poke(true.B)
      c.io.stop.poke(false.B)
      c.clock.step(1)
      c.io.timeout.expect(false.B)

      c.io.start.poke(false.B)
      c.clock.step(4)
      c.io.timeout.expect(true.B)
    }
  }

  "SimpleTimer" should "stop early" in {
    simulate(new SimpleTimer(10)) { c =>
      c.io.start.poke(true.B)
      c.clock.step(3)
      c.io.stop.poke(true.B)
      c.clock.step(10)
      c.io.timeout.expect(false.B)
    }
  }

  "DynamicTimer" should "use dynamic period" in {
    simulate(new DynamicTimer(8)) { c =>
      // Start timer with period=3
      c.io.period.poke(3.U)
      c.io.start.poke(true.B)
      c.io.stop.poke(false.B)
      c.clock.step(1)

      // Deactivate start - timer now counts down
      c.io.start.poke(false.B)
      c.clock.step(1)
      c.io.timeout.expect(false.B)

      c.clock.step(1)
      c.io.timeout.expect(false.B)

      c.clock.step(1)
      // Timeout when countdown reaches 0 (after 3 more cycles)
      c.io.timeout.expect(true.B)
    }
  }

  "DynamicTimer" should "stop before timeout" in {
    simulate(new DynamicTimer(8)) { c =>
      c.io.period.poke(10.U)
      c.io.start.poke(true.B)
      c.clock.step(1)
      c.io.start.poke(false.B)
      c.clock.step(5)
      c.io.stop.poke(true.B)
      c.clock.step(10)
      c.io.timeout.expect(false.B)
    }
  }

  "Timer" should "compile" in {
    assertCompiles("new Timer(100, 4)")
  }
}