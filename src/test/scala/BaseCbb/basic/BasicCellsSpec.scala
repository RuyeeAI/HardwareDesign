package BaseCbb.basic

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BasicCellsSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // ---- Combinational gates ----

  "Inv" should "invert input" in {
    test(new Inv) { c =>
      c.io.in.poke(true.B); c.io.out.expect(false.B)
      c.io.in.poke(false.B); c.io.out.expect(true.B)
    }
  }

  "Buf" should "pass input through" in {
    test(new Buf) { c =>
      c.io.in.poke(true.B); c.io.out.expect(true.B)
      c.io.in.poke(false.B); c.io.out.expect(false.B)
    }
  }

  "And2" should "compute AND" in {
    test(new And2) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.y.expect(false.B)
      c.io.a.poke(false.B); c.io.b.poke(true.B);  c.io.y.expect(false.B)
      c.io.a.poke(true.B);  c.io.b.poke(false.B); c.io.y.expect(false.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.y.expect(true.B)
    }
  }

  "And3" should "compute AND" in {
    test(new And3) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.c.poke(false.B); c.io.y.expect(false.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.c.poke(true.B);  c.io.y.expect(true.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.c.poke(false.B); c.io.y.expect(false.B)
    }
  }

  "Nand2" should "compute NAND" in {
    test(new Nand2) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.y.expect(true.B)
      c.io.a.poke(false.B); c.io.b.poke(true.B);  c.io.y.expect(true.B)
      c.io.a.poke(true.B);  c.io.b.poke(false.B); c.io.y.expect(true.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.y.expect(false.B)
    }
  }

  "Nand3" should "compute NAND" in {
    test(new Nand3) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.c.poke(false.B); c.io.y.expect(true.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.c.poke(true.B);  c.io.y.expect(false.B)
    }
  }

  "Or2" should "compute OR" in {
    test(new Or2) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.y.expect(false.B)
      c.io.a.poke(false.B); c.io.b.poke(true.B);  c.io.y.expect(true.B)
      c.io.a.poke(true.B);  c.io.b.poke(false.B); c.io.y.expect(true.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.y.expect(true.B)
    }
  }

  "Nor2" should "compute NOR" in {
    test(new Nor2) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.y.expect(true.B)
      c.io.a.poke(false.B); c.io.b.poke(true.B);  c.io.y.expect(false.B)
      c.io.a.poke(true.B);  c.io.b.poke(false.B); c.io.y.expect(false.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.y.expect(false.B)
    }
  }

  "Nor3" should "compute NOR" in {
    test(new Nor3) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.c.poke(false.B); c.io.y.expect(true.B)
      c.io.a.poke(true.B);  c.io.b.poke(false.B); c.io.c.poke(false.B); c.io.y.expect(false.B)
    }
  }

  "Xor2" should "compute XOR" in {
    test(new Xor2) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.y.expect(false.B)
      c.io.a.poke(false.B); c.io.b.poke(true.B);  c.io.y.expect(true.B)
      c.io.a.poke(true.B);  c.io.b.poke(false.B); c.io.y.expect(true.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.y.expect(false.B)
    }
  }

  "Xnor2" should "compute XNOR" in {
    test(new Xnor2) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.y.expect(true.B)
      c.io.a.poke(false.B); c.io.b.poke(true.B);  c.io.y.expect(false.B)
      c.io.a.poke(true.B);  c.io.b.poke(false.B); c.io.y.expect(false.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.y.expect(true.B)
    }
  }


  "Mux2N" should "select N-bit input" in {
    test(new Mux2N(UInt(8.W))) { c =>
      c.io.d0.poke(0xAB.U); c.io.d1.poke(0xCD.U)
      c.io.sel.poke(false.B); c.io.y.expect(0xAB.U)
      c.io.sel.poke(true.B);  c.io.y.expect(0xCD.U)
    }
  }



  "HalfAdd" should "add two bits" in {
    test(new HalfAdd) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.sum.expect(false.B); c.io.cout.expect(false.B)
      c.io.a.poke(false.B); c.io.b.poke(true.B);  c.io.sum.expect(true.B);  c.io.cout.expect(false.B)
      c.io.a.poke(true.B);  c.io.b.poke(false.B); c.io.sum.expect(true.B);  c.io.cout.expect(false.B)
      c.io.a.poke(true.B);  c.io.b.poke(true.B);  c.io.sum.expect(false.B); c.io.cout.expect(true.B)
    }
  }

  "FullAdd" should "add three bits" in {
    test(new FullAdd) { c =>
      c.io.a.poke(false.B); c.io.b.poke(false.B); c.io.cin.poke(false.B)
      c.io.sum.expect(false.B); c.io.cout.expect(false.B)
      c.io.a.poke(true.B); c.io.b.poke(true.B); c.io.cin.poke(true.B)
      c.io.sum.expect(true.B); c.io.cout.expect(true.B)
      c.io.a.poke(true.B); c.io.b.poke(false.B); c.io.cin.poke(false.B)
      c.io.sum.expect(true.B); c.io.cout.expect(false.B)
    }
  }

  "AOI22" should "compute AND-OR-INVERT" in {
    test(new AOI22) { c =>
      // all false: (0&0)|(0&0) = 0, !0 = 1
      c.io.a1.poke(false.B); c.io.a2.poke(false.B); c.io.b1.poke(false.B); c.io.b2.poke(false.B)
      c.io.y.expect(true.B)
      // a1=1, a2=1: (1&1)|(0&0) = 1, !1 = 0
      c.io.a1.poke(true.B); c.io.a2.poke(true.B); c.io.b1.poke(false.B); c.io.b2.poke(false.B)
      c.io.y.expect(false.B)
      // b1=1, b2=1: (0&0)|(1&1) = 1, !1 = 0
      c.io.a1.poke(false.B); c.io.a2.poke(false.B); c.io.b1.poke(true.B); c.io.b2.poke(true.B)
      c.io.y.expect(false.B)
    }
  }

  "AOI32" should "compute AND-OR-INVERT" in {
    test(new AOI32) { c =>
      c.io.a1.poke(false.B); c.io.a2.poke(false.B); c.io.a3.poke(false.B)
      c.io.b1.poke(false.B); c.io.b2.poke(false.B)
      c.io.y.expect(true.B)
      c.io.a1.poke(true.B); c.io.a2.poke(true.B); c.io.a3.poke(true.B)
      c.io.y.expect(false.B)
      c.io.b1.poke(true.B); c.io.b2.poke(true.B)
      c.io.y.expect(false.B)
    }
  }

  // DLatch and SRLatch contain intentional combinational feedback loops
  // which FIRRTL's CheckCombLoops rejects during simulation.
  // These are synthesizable latches but require vendor-specific latch
  // inference or BlackBox wrappers for verification.
}
