package BaseCbb.memory

import BaseCbb.memory.Memory
import BaseCbb.memory.MemoryProtectType
import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DebugInj2Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  "Debug2" should "read without poking inj ports" in {
    test(new SpMemoryWrap3(
      Memory(name="D2", dataType=UInt(32.W), depth=64, protect=MemoryProtectType.ECC, flopIn=false, flopOut=false)
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.lgc.we.poke(false.B)
      c.io.lgc.re.poke(false.B)
      c.clock.step(2)

      c.io.lgc.we.poke(true.B)
      c.io.lgc.addr.poke(0.U)
      c.io.lgc.wdata.poke(0xABCD5678L.U)
      c.clock.step(1)
      c.io.lgc.we.poke(false.B)
      c.clock.step(1)

      c.io.lgc.re.poke(true.B)
      c.io.lgc.addr.poke(0.U)
      c.clock.step(1)
      c.io.lgc.re.poke(false.B)
      c.clock.step(10)

      c.io.lgc.rdata.expect(0xABCD5678L.U)
    }
  }
}
