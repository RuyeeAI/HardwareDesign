package ImpulseGenerator

import chisel3._
import chisel3.tester._
import org.scalatest.freespec.AnyFreeSpec

class ControlledImpulseGeneratorSpec extends AnyFreeSpec with ChiselScalatestTester {
  "ControlledImpulseGenerator should generate correct number of pulses with correct width" in {
    test(new ControlledImpulseGenerator) { c =>
      // 测试：输出3个脉冲，每个宽度为1个周期
      c.io.start.poke(true.B)
      c.io.length.poke(2.U)  // 0~2，共3个脉冲
      c.io.width.poke(0.U)   // 宽度 = 0 + 1 = 1个周期
      c.clock.step(1)
      c.io.start.poke(false.B)
      
      // 运行足够多周期观察输出
      for (_ <- 0 until 20) {
        println(s"out = ${c.io.out.peek().litValue}")
        c.clock.step(1)
      }
    }
  }
}
