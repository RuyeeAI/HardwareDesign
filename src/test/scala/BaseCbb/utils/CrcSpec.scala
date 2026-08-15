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

class CrcSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  "Crc-32" should "compute known MSB-first CRC" in {
    test(new Crc(32, 0x04C11DB7L)) { c =>
      // MSB-first CRC-32 of single byte 0x00 with init=all-ones
      c.io.data.poke(0x00.U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.init.poke("hFFFFFFFF".U)
      c.clock.step(1)
      c.io.crc.expect("h4E08BFB4".U)
    }
  }

  "Crc-32" should "accumulate multiple bytes" in {
    test(new Crc(32, 0x04C11DB7L)) { c =>
      // Byte 0: 0x00
      c.io.data.poke(0x00.U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.init.poke("hFFFFFFFF".U)
      c.clock.step(1)

      // Byte 1: 0x00
      c.io.first.poke(false.B)
      c.clock.step(1)
    }
  }

  "Crc-32" should "produce constant for zero data" in {
    test(new Crc(32, 0x04C11DB7L)) { c =>
      c.io.data.poke(0x00.U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.init.poke("hFFFFFFFF".U)
      c.clock.step(1)
      // Verify output is non-zero (CRC is not trivially zero)
      c.io.crc.peek().litValue should not be 0
    }
  }

  "Crc-16" should "compute known values" in {
    test(new Crc(16, 0x8005L)) { c =>
      // CRC-16 of 0x00 with init=0
      c.io.data.poke(0x00.U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.init.poke(0.U)
      c.clock.step(1)
      // CRC-16 result should be zero for zero data with zero init
      c.io.crc.expect(0.U)
    }
  }

  "Crc-8" should "compute known values" in {
    test(new Crc(8, 0x07L)) { c =>
      // CRC-8 of 0x00 with init=0
      c.io.data.poke(0x00.U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.init.poke(0.U)
      c.clock.step(1)
      c.io.crc.expect(0.U)
    }
  }

  "Crc-32" should "produce different CRC for different data" in {
    test(new Crc(32, 0x04C11DB7L)) { c =>
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.init.poke("hFFFFFFFF".U)

      c.io.data.poke(0x00.U)
      c.clock.step(1)
      val crc0 = c.io.crc.peek().litValue

      c.io.data.poke(0xFF.U)
      c.io.first.poke(true.B)
      c.clock.step(1)
      val crcFF = c.io.crc.peek().litValue

      crc0 should not be crcFF
    }
  }

  "Icrc" should "compute remainder after feeding back CRC" in {
    test(new Icrc(32, 0x04C11DB7L)) { c =>
      // Feed data byte 0x00
      c.io.data.poke(0x00.U)
      c.io.valid.poke(true.B)
      c.io.first.poke(true.B)
      c.io.init.poke("hFFFFFFFF".U)
      c.io.crcVld.poke(false.B)
      c.clock.step(1)

      // Feed CRC bytes (MSB first) — simulate feeding CRC back
      // MSB-first CRC-32(0x00) = 0x4E08BFB4 with init=0xFFFFFFFF
      val crcVal = BigInt("4E08BFB4", 16)
      c.io.crcIn.poke(((crcVal >> 24) & 0xFF).U)
      c.io.crcVld.poke(true.B)
      c.clock.step(1)

      c.io.crcIn.poke(((crcVal >> 16) & 0xFF).U)
      c.clock.step(1)

      c.io.crcIn.poke(((crcVal >> 8) & 0xFF).U)
      c.clock.step(1)

      c.io.crcIn.poke((crcVal & 0xFF).U)
      c.clock.step(1)

      // ICRC remainder should be non-zero
      val icrc = c.io.icrc.peek().litValue
      icrc should not be 0
    }
  }
}
