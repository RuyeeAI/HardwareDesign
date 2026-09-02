package BaseCbb.RegCbb

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import BaseCbb.RegCbb.demo.UartDemoDef
import BaseCbb.RegCbb.dsl._
import BaseCbb.RegCbb.hw._

/**
 * ★ 决定性实验：entry 字段在硬件中的真实位位置。
 * 用 FieldReg 例化 entry 字段（来自 toEntryFields），按字段 poke hwWrData，
 * 观察整个 value 中该字段占据的 bit 位置 —— 而非整字写读（无法区分字段位）。
 */
class EntryFieldBitPosTest extends AnyFreeSpec with ChiselScalatestTester {

  private class EntryProbe(fields: Seq[RegFieldDef]) extends Module {
    val io = IO(new Bundle {
      val wr = Input(Bool()); val wdata = Input(UInt(32.W)); val rd = Input(Bool())
      val rdata = Output(UInt(32.W)); val value = Output(UInt(32.W))
      // 逐字段写接口（RW 字段直写）：fieldData(i) 写第 i 个字段
      val fieldData = Input(Vec(fields.size, UInt(32.W)))
      val fieldEn = Input(Bool())
    })
    val reg = RegDef("entry_reg", fields)
    val alloc = AddressAllocator.allocateRegisters(Seq(reg)).head
    val fr = Module(new FieldReg(alloc, 32))
    fr.io.dec.wr := io.wr
    fr.io.dec.wdata := io.wdata
    fr.io.dec.rd := io.rd
    fr.io.wordSel := 0.U
    fr.io.core.hwWr.en := io.fieldEn
    fr.io.core.ro.value.elements.foreach { case (_, v) => v := 0.U(v.getWidth.W) }
    fr.io.core.hwSet.bits.elements.foreach   { case (_, v) => v := 0.U(v.getWidth.W) }
    fr.io.core.hwClr.bits.elements.foreach   { case (_, v) => v := 0.U(v.getWidth.W) }
    fr.io.core.hwTog.bits.elements.foreach   { case (_, v) => v := 0.U(v.getWidth.W) }
    // hwWrData：逐字段连到 io.fieldData（按字段顺序）
    fr.io.core.hwWr.data.elements.zipWithIndex.foreach { case ((n, v), i) =>
      v := io.fieldData(i)(v.getWidth - 1, 0)
    }
    io.rdata := fr.io.dec.rdata
    io.value := fr.io.core.sw.value
  }

  "FifoDescEntry 各字段在硬件中的真实 bit 位置（逐字段 poke）" in {
    val fields = BundleToRegDefs.toEntryFields(new UartDemoDef.FifoDescEntry)
    // 确认字段序列与位偏移（IR 层）
    info(s"entryFields = ${fields.map(f => f.name -> f.bitWidth).mkString(", ")}")
    val mem = MemoryDef.fromBundle("probe", 4, fields)
    info(s"entryFieldOffsets = ${mem.entryFieldOffsets.mkString(", ")}")

    test(new EntryProbe(fields)) { c =>
      // 逐个字段写全 1，观察 value 中该字段占据的 bit 位置
      fields.zipWithIndex.foreach { case (f, idx) =>
        c.io.fieldEn.poke(true.B)
        fields.indices.foreach { i =>
          c.io.fieldData(i).poke(if (i == idx) ((BigInt(1) << f.bitWidth) - 1).U else 0.U)
        }
        c.clock.step(1)
        c.io.fieldEn.poke(false.B)
        c.clock.step(1)
        val v = c.io.value.peek().litValue
        // 该字段全 1 出现在 value 的哪个位区间（按声明序 LSB-first）
        val lo = fields.take(idx).map(_.bitWidth).sum
        val hi = lo + f.bitWidth - 1
        val expect = ((BigInt(1) << f.bitWidth) - 1) << lo
        info(s"字段 ${f.name} (宽${f.bitWidth}): value=0x${v.toString(16)} 期望位区间[${hi}:${lo}] 期望值=0x${expect.toString(16)}")
        assert(v == expect,
          s"${f.name} 硬件位位置错误：value=0x${v.toString(16)}，期望(按声明序 LSB-first)=0x${expect.toString(16)}")
      }
    }
  }
}
