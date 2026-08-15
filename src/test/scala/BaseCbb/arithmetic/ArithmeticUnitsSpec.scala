package BaseCbb.arithmetic

import BaseCbb.math.{AddSub, CarrySelectAdder, Comparator, LeftShifter, Multipler, RightShifter, RippleCarryAdder, Subtractor}
import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArithmeticUnitsSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ---- RippleCarryAdder ----

  "RippleCarryAdder" should "add without carry" in {
    test(new RippleCarryAdder(8)) { c =>
      c.io.a.poke(5.U)
      c.io.b.poke(3.U)
      c.io.cin.poke(false.B)
      c.io.sum.expect(8.U)
      c.io.cout.expect(false.B)
    }
  }

  "RippleCarryAdder" should "add with carry in" in {
    test(new RippleCarryAdder(8)) { c =>
      c.io.a.poke(5.U)
      c.io.b.poke(3.U)
      c.io.cin.poke(true.B)
      c.io.sum.expect(9.U)
      c.io.cout.expect(false.B)
    }
  }

  "RippleCarryAdder" should "produce carry out on overflow" in {
    test(new RippleCarryAdder(8)) { c =>
      c.io.a.poke(0xFF.U)
      c.io.b.poke(1.U)
      c.io.cin.poke(false.B)
      c.io.sum.expect(0.U)
      c.io.cout.expect(true.B)
    }
  }

  "RippleCarryAdder" should "add with max values" in {
    test(new RippleCarryAdder(8)) { c =>
      c.io.a.poke(0xF0.U)
      c.io.b.poke(0x0F.U)
      c.io.cin.poke(false.B)
      c.io.sum.expect(0xFF.U)
      c.io.cout.expect(false.B)
    }
  }

  // ---- CarrySelectAdder ----

  "CarrySelectAdder" should "add correctly" in {
    test(new CarrySelectAdder(16, 4)) { c =>
      c.io.a.poke(100.U)
      c.io.b.poke(200.U)
      c.io.cin.poke(false.B)
      c.io.sum.expect(300.U)
      c.io.cout.expect(false.B)
    }
  }

  "CarrySelectAdder" should "produce carry out" in {
    test(new CarrySelectAdder(8, 4)) { c =>
      c.io.a.poke(0xFF.U)
      c.io.b.poke(1.U)
      c.io.cin.poke(false.B)
      c.io.sum.expect(0.U)
      c.io.cout.expect(true.B)
    }
  }

  "CarrySelectAdder" should "add with carry in" in {
    test(new CarrySelectAdder(8, 4)) { c =>
      c.io.a.poke(10.U)
      c.io.b.poke(20.U)
      c.io.cin.poke(true.B)
      c.io.sum.expect(31.U)
    }
  }

  // ---- Subtractor ----

  "Subtractor" should "subtract" in {
    test(new Subtractor(8)) { c =>
      c.io.a.poke(10.U)
      c.io.b.poke(3.U)
      c.io.diff.expect(7.U)
      c.io.borrowOut.expect(true.B) // no borrow
    }
  }

  "Subtractor" should "handle underflow" in {
    test(new Subtractor(8)) { c =>
      c.io.a.poke(3.U)
      c.io.b.poke(10.U)
      c.io.diff.expect(249.U) // 3 - 10 = -7 -> 249 in 8-bit unsigned
      c.io.borrowOut.expect(false.B)
    }
  }

  // ---- AddSub ----

  "AddSub" should "add when sub=0" in {
    test(new AddSub(8)) { c =>
      c.io.a.poke(10.U)
      c.io.b.poke(5.U)
      c.io.sub.poke(false.B)
      c.io.result.expect(15.U)
    }
  }

  "AddSub" should "subtract when sub=1" in {
    test(new AddSub(8)) { c =>
      c.io.a.poke(10.U)
      c.io.b.poke(3.U)
      c.io.sub.poke(true.B)
      c.io.result.expect(7.U)
    }
  }

  // ---- Comparator ----

  "Comparator" should "compare correctly" in {
    test(new Comparator(8)) { c =>
      c.io.a.poke(10.U); c.io.b.poke(5.U)
      c.io.eq.expect(false.B); c.io.gt.expect(true.B); c.io.lt.expect(false.B)

      c.io.a.poke(5.U); c.io.b.poke(10.U)
      c.io.eq.expect(false.B); c.io.gt.expect(false.B); c.io.lt.expect(true.B)

      c.io.a.poke(7.U); c.io.b.poke(7.U)
      c.io.eq.expect(true.B); c.io.gt.expect(false.B); c.io.lt.expect(false.B)
    }
  }

  // ---- Multipler ----

  "Multipler" should "multiply" in {
    test(new Multipler(8, 8)) { c =>
      c.io.a.poke(6.U)
      c.io.b.poke(7.U)
      c.io.product.expect(42.U)
    }
  }

  "Multipler" should "multiply by zero" in {
    test(new Multipler(8, 8)) { c =>
      c.io.a.poke(100.U)
      c.io.b.poke(0.U)
      c.io.product.expect(0.U)
    }
  }

  "Multipler" should "multiply max values" in {
    test(new Multipler(4, 4)) { c =>
      c.io.a.poke(15.U)
      c.io.b.poke(15.U)
      c.io.product.expect(225.U)
    }
  }

  // ---- LeftShifter ----

  "LeftShifter" should "shift left" in {
    test(new LeftShifter(8)) { c =>
      c.io.din.poke(0x01.U)
      c.io.shamt.poke(3.U)
      c.io.dout.expect(0x08.U)
    }
  }

  "LeftShifter" should "shift by zero" in {
    test(new LeftShifter(8)) { c =>
      c.io.din.poke(0xAB.U)
      c.io.shamt.poke(0.U)
      c.io.dout.expect(0xAB.U)
    }
  }

  "LeftShifter" should "truncate on overflow" in {
    test(new LeftShifter(8)) { c =>
      c.io.din.poke(0x80.U)
      c.io.shamt.poke(1.U)
      c.io.dout.expect(0x00.U) // 0x100 & 0xFF
    }
  }

  // ---- RightShifter ----

  "RightShifter (logical)" should "shift right" in {
    test(new RightShifter(8, false)) { c =>
      c.io.din.poke(0x80.U)
      c.io.shamt.poke(4.U)
      c.io.dout.expect(0x08.U)
    }
  }

  "RightShifter (logical)" should "zero-fill MSB" in {
    test(new RightShifter(8, false)) { c =>
      c.io.din.poke(0xFF.U)
      c.io.shamt.poke(2.U)
      c.io.dout.expect(0x3F.U)
    }
  }

  "RightShifter (arithmetic)" should "sign-extend MSB" in {
    test(new RightShifter(8, true)) { c =>
      // 0x80 = 10000000 as signed = -128
      c.io.din.poke(0x80.U)
      c.io.shamt.poke(3.U)
      // -128 >> 3 = -16 = 0xF0
      c.io.dout.expect(0xF0.U)
    }
  }
}
