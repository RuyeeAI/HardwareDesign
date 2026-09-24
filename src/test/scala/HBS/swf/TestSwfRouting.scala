package HBS.swf

import HBS.swf.common.SwfParams
import HBS.swf.sfu_routing.SfuRouting
import BaseCbb.Sim._
import chisel3._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

/** 由 HBS `src/test/scala/swf/TestSwfRouting.scala` 迁入（包名 swf → HBS.swf）。 */
class TestSwfRouting extends AnyFreeSpec with Matchers {
  "SwfRoutingSanity" in {
    simulate(new SfuRouting(new SwfParams)) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()
      dut.ib_swf_intf(0).ib_swf_data(0).bits.data.bits.poke(1.U)
      dut.ib_swf_intf(0).ib_swf_data(0).bits.data.valid.poke(true.B)
    }
  }
}
