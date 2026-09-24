package em

import chisel3._
import BaseCbb.Sim._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** 固化 CRC 探针：直接例化 Crc.hardwired，用于与 Scala 参考模型逐值比对。 */
class CrcHwProbe(dataW: Int, crcW: Int, m: CrcHardwired) extends Module {
  val io = IO(new Bundle {
    val din = Input(UInt(dataW.W))
    val out = Output(UInt(crcW.W))
  })
  io.out := Crc.hardwired(io.din, crcW, m.poly, m.init, m.xorout, m.refin, m.refout)
}

/** 串行 CRC 探针（CrcRuntime 形态）。 */
class CrcSerProbe(dataW: Int, crcW: Int, m: CrcRuntime) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val din   = Input(UInt(dataW.W))
    val out   = Output(UInt(crcW.W))
    val done  = Output(Bool())
  })
  val c = Module(new CrcSerial(crcW, dataW, m.refin, m.refout))
  c.io.start := io.start
  c.io.din   := io.din
  c.io.poly  := m.poly.U(crcW.W)
  c.io.init  := m.init.U(crcW.W)
  c.io.xor   := m.xorout.U(crcW.W)
  io.out  := c.io.out
  io.done := c.io.done
}

// ===========================================================================
// CRC 用例
//
// 1) 等价性：改写后的平衡 XOR 树必须与**旧的逐位串行实现**逐值一致
//    （CRC 对输入位是线性的，改写只是把 2n 级串行链换成 ⌈log2(n)⌉ 级 XOR 树）
// 2) 标准锚点：reflected CRC-32("123456789") = 0xCBF43926，钉住数值约定
// 3) 串行 CRC 与固化 CRC 同参数下一致（同时验证 CrcSerial 的 done/out 同拍）
// ===========================================================================
class CrcSpec extends AnyFlatSpec with Matchers {

  /** 旧实现的 Scala 参考模型（逐位串行，与改写前的 Crc.hardwired 等价） */
  private def refCrc(data: BigInt, dataW: Int, crcW: Int, poly: BigInt, init: BigInt,
                     xorout: BigInt, refin: Boolean, refout: Boolean): BigInt = {
    var st: BigInt = init
    val order = if (refin) (0 until dataW) else (0 until dataW).reverse
    for (p <- order) {
      val b: Int = ((data >> p) & 1).toInt
      val fb: Int = ((st >> (crcW - 1)) & 1).toInt ^ b
      var ns = BigInt(0)
      for (i <- 0 until crcW) {
        val shifted: Int = if (i == 0) 0 else ((st >> (i - 1)) & 1).toInt
        val v: Int = shifted ^ (if (((poly >> i) & 1) == 1) fb else 0)
        if (v == 1) ns |= (BigInt(1) << i)
      }
      st = ns
    }
    var o = BigInt(0)
    for (i <- 0 until crcW) {
      val srcBit = if (refout) (st >> (crcW - 1 - i)) & 1 else (st >> i) & 1
      if (srcBit == 1) o |= (BigInt(1) << i)
    }
    (o ^ xorout) & ((BigInt(1) << crcW) - 1)
  }

  private val KEYS: Seq[BigInt] = {
    val fixed = Seq(
      BigInt(0), BigInt(1), BigInt(2), BigInt("FFFFFFFFFFFF", 16),
      BigInt("800000000000", 16), BigInt("000000000001", 16),
      BigInt("0A0B0C0D0E0F", 16), BigInt("010203040506", 16),
      BigInt("AAAAAAAAAAAA", 16), BigInt("555555555555", 16)
    )
    val rnd = new scala.util.Random(0x5EED)
    fixed ++ (0 until 20).map(_ => BigInt(48, rnd).mod(BigInt(1) << 48))
  }

  "Crc.hardwired（平衡 XOR 树）" should "与逐位串行参考模型逐值一致（reflected CRC-32）" in {
    val m = CrcHardwired.crc32
    simulate(new CrcHwProbe(48, 32, m)) { dut =>
      KEYS.foreach { k =>
        dut.io.din.poke(k.U)
        dut.clock.step(1)
        val got = dut.io.out.peek().litValue
        val exp = refCrc(k, 48, 32, m.poly, m.init, m.xorout, m.refin, m.refout)
        withClue(f"key=0x$k%012X: ")(got shouldBe exp)
      }
    }
  }

  "Crc.hardwired" should "与逐位串行参考模型一致（非反射形态 refin/refout=false）" in {
    val m = CrcHardwired(BigInt("04C11DB7", 16))     // init/xorout=0, refin/refout=false
    simulate(new CrcHwProbe(48, 32, m)) { dut =>
      KEYS.take(12).foreach { k =>
        dut.io.din.poke(k.U)
        dut.clock.step(1)
        dut.io.out.peek().litValue shouldBe refCrc(k, 48, 32, m.poly, m.init, m.xorout, m.refin, m.refout)
      }
    }
  }

  "Crc.hardwired" should "命中标准 CRC-32 check value 0xCBF43926（钉住数值约定）" in {
    val m = CrcHardwired.crc32
    val bytes = "123456789".getBytes("US-ASCII")
    val data  = bytes.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (b, i)) =>
      acc | (BigInt(b & 0xFF) << (8 * i))
    }
    simulate(new CrcHwProbe(72, 32, m)) { dut =>
      dut.io.din.poke(data.U)
      dut.clock.step(1)
      dut.io.out.peek().litValue shouldBe BigInt("CBF43926", 16)
    }
  }

  "CrcSerial（CrcRuntime 形态）" should "与固化 CRC 同参数下逐值一致（验证 done/out 同拍）" in {
    val hw = CrcHardwired.crc32
    val rt = CrcRuntime.crc32
    val dataW = 16
    val keys = KEYS.map(_ & 0xFFFF)
    val exp  = keys.map(k => refCrc(k, dataW, 32, hw.poly, hw.init, hw.xorout, hw.refin, hw.refout))
    simulate(new CrcSerProbe(dataW, 32, rt)) { dut =>
      dut.io.start.poke(false.B)
      dut.io.din.poke(0.U)
      dut.clock.step(2)
      keys.zip(exp).foreach { case (k, e) =>
        dut.io.din.poke(k.U)
        dut.io.start.poke(true.B)
        dut.clock.step(1)                      // start 握手
        dut.io.start.poke(false.B)
        var n = 0
        while (dut.io.done.peek().litValue == 0 && n < 200) { dut.clock.step(1); n += 1 }
        dut.io.done.peek().litValue shouldBe 1
        withClue(f"key=0x$k%04X: ")(dut.io.out.peek().litValue shouldBe e)
        dut.clock.step(1)
      }
    }
  }
}
