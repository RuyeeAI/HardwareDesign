package BaseCbb.memory

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import BaseCbb.memory.Memory
import BaseCbb.memory.MemoryProtectType

class DebugInjSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  "Debug" should "basic read works with injCorrEn=0" in {
    test(new SpMemoryWrap3(
      Memory(name="D1", dataType=UInt(32.W), depth=64, protect=MemoryProtectType.ECC, flopIn=false, flopOut=false)
    )).withAnnotations(Seq()) { c =>
      c.reset.poke(false.B)
      c.io.dfx.init.poke(false.B)
      c.io.dfx.injCorrEn.poke(false.B)
      c.io.dfx.injUerrEn.poke(false.B)
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
      // injCorrEn stays 0
      c.clock.step(1)
      c.io.lgc.re.poke(false.B)
      c.clock.step(10)

      println(s"rdata=${c.io.lgc.rdata.peek().litValue}")
      println(s"eccErr=${c.io.dfx.eccErr.peek().litValue}")
      c.io.lgc.rdata.expect(0xABCD5678L.U)
    }
  }
}
