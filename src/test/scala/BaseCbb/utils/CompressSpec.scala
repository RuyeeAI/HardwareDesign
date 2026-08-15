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
import chisel3.util._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompressSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  // Convert a Seq[Boolean] to an integer bitmask (element 0 = LSB)
  private def boolMask(bits: Seq[Boolean]): BigInt =
    bits.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (b, i)) => if (b) acc | (BigInt(1) << i) else acc }

  // Scala reference model: compress valid elements to LSB
  private def compressRef(in: Seq[Int], valid: Seq[Boolean]): (Seq[Int], Int) = {
    val kept = in.zip(valid).collect { case (v, true) => v }
    val zeros = Seq.fill(in.size - kept.size)(0)
    (kept ++ zeros, kept.size)
  }

  // ---- Compress basic cases ----

  "Compress N=8" should "pack all valid elements to LSB" in {
    test(new Compress(UInt(8.W), 8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val data = Seq(0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x11, 0x22)
      val valid = Seq(true, false, true, false, true, false, true, false)

      c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
      c.io.valid.poke(boolMask(valid).U)
      c.clock.step(1)

      c.io.out(0).expect(0xAA.U)
      c.io.out(1).expect(0xCC.U)
      c.io.out(2).expect(0xEE.U)
      c.io.out(3).expect(0x11.U)
      c.io.out(4).expect(0.U)
      c.io.out(5).expect(0.U)
      c.io.out(6).expect(0.U)
      c.io.out(7).expect(0.U)
      c.io.count.expect(4.U)
    }
  }

  "Compress" should "output all zeros when no elements are valid" in {
    test(new Compress(UInt(8.W), 8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val data = Seq(0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x11, 0x22)
      c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
      c.io.valid.poke(0.U)
      c.clock.step(1)

      for (j <- 0 until 8) {
        c.io.out(j).expect(0.U)
      }
      c.io.count.expect(0.U)
    }
  }

  "Compress" should "pass through when all elements are valid" in {
    test(new Compress(UInt(8.W), 8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val data = Seq(0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08)
      c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
      c.io.valid.poke(((1 << 8) - 1).U)
      c.clock.step(1)

      for (j <- 0 until 8) {
        c.io.out(j).expect(data(j).U)
      }
      c.io.count.expect(8.U)
    }
  }

  "Compress" should "work with only LSB valid" in {
    test(new Compress(UInt(8.W), 8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val data = Seq(0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x11, 0x22)
      c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
      c.io.valid.poke(1.U)
      c.clock.step(1)

      c.io.out(0).expect(0xAA.U)
      for (j <- 1 until 8) {
        c.io.out(j).expect(0.U)
      }
      c.io.count.expect(1.U)
    }
  }

  "Compress" should "work with only MSB valid" in {
    test(new Compress(UInt(8.W), 8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val data = Seq(0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x11, 0x22)
      c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
      c.io.valid.poke((1 << 7).U)
      c.clock.step(1)

      c.io.out(0).expect(0x22.U)
      for (j <- 1 until 8) {
        c.io.out(j).expect(0.U)
      }
      c.io.count.expect(1.U)
    }
  }

  "Compress N=1" should "work with valid=0" in {
    test(new Compress(UInt(8.W), 1)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.in(0).poke(0x42.U)
      c.io.valid.poke(0.U)
      c.clock.step(1)
      c.io.out(0).expect(0.U)
      c.io.count.expect(0.U)
    }
  }

  "Compress N=1" should "work with valid=1" in {
    test(new Compress(UInt(8.W), 1)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.in(0).poke(0x42.U)
      c.io.valid.poke(1.U)
      c.clock.step(1)
      c.io.out(0).expect(0x42.U)
      c.io.count.expect(1.U)
    }
  }

  // ---- Scatter basic cases ----

  "Scatter N=8" should "place elements at mask positions" in {
    test(new Scatter(UInt(8.W), 8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val data = Seq(0xA, 0xB, 0xC, 0xD, 0xE, 0xF, 0x1, 0x2)
      val mask = Seq(true, false, true, false, true, false, true, false)

      c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
      c.io.mask.poke(boolMask(mask).U)
      c.clock.step(1)

      c.io.out(0).expect(0xA.U)
      c.io.out(1).expect(0.U)
      c.io.out(2).expect(0xB.U)
      c.io.out(3).expect(0.U)
      c.io.out(4).expect(0xC.U)
      c.io.out(5).expect(0.U)
      c.io.out(6).expect(0xD.U)
      c.io.out(7).expect(0.U)
    }
  }

  "Scatter" should "output all zeros with zero mask" in {
    test(new Scatter(UInt(8.W), 8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val data = Seq(0xA, 0xB, 0xC, 0xD, 0xE, 0xF, 0x1, 0x2)
      c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
      c.io.mask.poke(0.U)
      c.clock.step(1)

      for (j <- 0 until 8) {
        c.io.out(j).expect(0.U)
      }
    }
  }

  "Scatter" should "pass through with all-ones mask" in {
    test(new Scatter(UInt(8.W), 8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val data = Seq(0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8)
      c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
      c.io.mask.poke(((1 << 8) - 1).U)
      c.clock.step(1)

      for (j <- 0 until 8) {
        c.io.out(j).expect(data(j).U)
      }
    }
  }

  "Scatter N=1" should "work with mask=0" in {
    test(new Scatter(UInt(8.W), 1)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.in(0).poke(0x42.U)
      c.io.mask.poke(0.U)
      c.clock.step(1)
      c.io.out(0).expect(0.U)
    }
  }

  "Scatter N=1" should "work with mask=1" in {
    test(new Scatter(UInt(8.W), 1)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.in(0).poke(0x42.U)
      c.io.mask.poke(1.U)
      c.clock.step(1)
      c.io.out(0).expect(0x42.U)
    }
  }

  // ---- Randomized regression ----

  private val testWidths = Seq(4, 8, 16, 32)

  testWidths.foreach { n =>
    s"Compress N=$n randomized" should "match reference model" in {
      test(new Compress(UInt(8.W), n)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val rng = new scala.util.Random(42)
        for (_ <- 0 until 100) {
          val data  = Seq.fill(n)(rng.nextInt(256) & 0xFF)
          val valid = Seq.fill(n)(rng.nextBoolean())

          c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
          c.io.valid.poke(boolMask(valid).U)
          c.clock.step(1)

          val (expectedOut, expectedCount) = compressRef(data, valid)
          for (j <- 0 until n) {
            c.io.out(j).expect(expectedOut(j).U)
          }
          c.io.count.expect(expectedCount.U)
        }
      }
    }
  }

  testWidths.foreach { n =>
    s"Scatter N=$n randomized" should "place elements at correct mask positions" in {
      test(new Scatter(UInt(8.W), n)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val rng = new scala.util.Random(123)
        for (_ <- 0 until 100) {
          val packed = Seq.fill(n)(rng.nextInt(256) & 0xFF)
          val mask   = Seq.fill(n)(rng.nextBoolean())

          c.io.in.zip(packed).foreach { case (port, v) => port.poke(v.U) }
          c.io.mask.poke(boolMask(mask).U)
          c.clock.step(1)

          var packedIdx = 0
          for (j <- 0 until n) {
            if (mask(j)) {
              c.io.out(j).expect(packed(packedIdx).U)
              packedIdx += 1
            } else {
              c.io.out(j).expect(0.U)
            }
          }
        }
      }
    }
  }

  // ---- Round-trip test ----

  "Compress + Scatter" should "recover original valid elements" in {
    test(new Compress(UInt(8.W), 4)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val rng = new scala.util.Random(99)
      for (_ <- 0 until 50) {
        val data  = Seq.fill(4)(rng.nextInt(256) & 0xFF)
        val valid = Seq.fill(4)(rng.nextBoolean())

        c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
        c.io.valid.poke(boolMask(valid).U)
        c.clock.step(1)

        // Read back compressed output and check
        val (expectedOut, expectedCount) = compressRef(data, valid)
        for (j <- 0 until 4) {
          c.io.out(j).expect(expectedOut(j).U)
        }
        c.io.count.expect(expectedCount.U)
      }
    }
  }

  // ---- Wider data types ----

  "Compress with UInt(32.W)" should "work correctly" in {
    test(new Compress(UInt(32.W), 4)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val data = Seq(0xDEADBEEFL, 0xCAFEBABEL, 0x12345678L, 0xABCDEF01L)
      val valid = Seq(true, false, true, false)

      c.io.in.zip(data).foreach { case (port, v) => port.poke(v.U) }
      c.io.valid.poke(boolMask(valid).U)
      c.clock.step(1)

      c.io.out(0).expect(0xDEADBEEFL.U)
      c.io.out(1).expect(0x12345678L.U)
      c.io.out(2).expect(0.U)
      c.io.out(3).expect(0.U)
      c.io.count.expect(2.U)
    }
  }
}
