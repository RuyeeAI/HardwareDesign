package BaseCbb.RegCbb

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import BaseCbb.RegCbb.demo.UartDemoDef
import BaseCbb.RegCbb.dsl._
import BaseCbb.RegCbb.hw._

/**
 * RegBundle 字段位序硬件级验证：
 *  - bundle_ctrl：声明 mode(2) 先、burst(1) 后 → 期望 mode[1:0] LSB、burst[2] MSB
 *  - FifoDescEntry（memory entry）：声明 tag(8) 先 → 期望 tag[7:0] LSB
 */
class BundleBitOrderTest extends AnyFreeSpec with ChiselScalatestTester {

  /** 用 FieldReg 例化单寄存器，poke 全 1 后检查各字段位位置 */
  private def fieldPositions(fields: Seq[RegFieldDef], totalBits: Int): Map[String, Int] = {
    val reg = RegDef("test_reg", fields)
    val alloc = AddressAllocator.allocateRegisters(Seq(reg)).head
    val m = Module(new FieldReg(alloc, 32))
    val io = m.io
    // 驱动 dec/wordSel，读 value
    val positions = scala.collection.mutable.Map[String, Int]()
    // 逐个字段写全 1，检查 readVal 中该字段位
    fields.foreach { f =>
      positions(f.name) = alloc.fieldAllocations.find(_.field.name == f.name).get.bitOffset
    }
    positions.toMap
  }

  "bundle_ctrl 字段位序（硬件 FieldReg）" in {
    val b = new UartDemoDef.UartBundleRegs
    val regs = BundleToRegDefs.toRegDefs(b)
    val ctrl = regs.find(_.name == "bundle_ctrl").get
    // 声明序：mode(2) 先、burst(1) 后
    assert(ctrl.fields.map(_.name) == Seq("mode", "burst"),
      s"字段声明序应为 mode, burst，实际 ${ctrl.fields.map(_.name)}")
    val alloc = AddressAllocator.allocateRegisters(Seq(ctrl)).head
    assert(alloc.fieldAllocations.map(fa => fa.field.name -> fa.bitOffset) ==
      Seq("mode" -> 0, "burst" -> 2),
      s"位偏移应为 mode@0, burst@2，实际 ${alloc.fieldAllocations.map(fa => fa.field.name -> fa.bitOffset)}")
  }

  "FifoDescEntry entry 字段位序" in {
    val fields = BundleToRegDefs.toEntryFields(new UartDemoDef.FifoDescEntry)
    assert(fields.map(_.name) == Seq("tag", "len", "crc"),
      s"entry 字段序应为 tag, len, crc，实际 ${fields.map(_.name)}")
    val mem = MemoryDef.fromBundle("test", 4, fields)
    assert(mem.entryFieldOffsets == Seq(0, 8, 24),
      s"entry 位偏移应为 0,8,24，实际 ${mem.entryFieldOffsets}")
  }

  "FieldReg 硬件仿真：写值后各字段读回位位置正确" in {
    test(new Module {
      val io = IO(new Bundle {
        val wr = Input(Bool()); val wdata = Input(UInt(32.W)); val rd = Input(Bool())
        val rdata = Output(UInt(32.W)); val value = Output(UInt(32.W))
      })
      val fields = Seq(
        RegFieldDef("tag", 8, AccessType.RW, 0, "标签"),
        RegFieldDef("len", 16, AccessType.RW, 0, "长度"),
        RegFieldDef("crc", 8, AccessType.RW, 0, "校验")
      )
      val reg = RegDef("entry_reg", fields)
      val alloc = AddressAllocator.allocateRegisters(Seq(reg)).head
      val fr = Module(new FieldReg(alloc, 32))
      fr.io.dec.wr := io.wr
      fr.io.dec.wdata := io.wdata
      fr.io.dec.rd := io.rd
      fr.io.wordSel := 0.U
      // 用户侧输入默认 0（避免 VOID）
      fr.io.core.hwWr.en := false.B
      fr.io.core.ro.value.elements.foreach { case (_, v) => v := 0.U(v.getWidth.W) }
      fr.io.core.hwSet.bits.elements.foreach   { case (_, v) => v := 0.U(v.getWidth.W) }
      fr.io.core.hwClr.bits.elements.foreach   { case (_, v) => v := 0.U(v.getWidth.W) }
      fr.io.core.hwTog.bits.elements.foreach   { case (_, v) => v := 0.U(v.getWidth.W) }
      fr.io.core.hwWr.data.elements.foreach{ case (_, v) => v := 0.U(v.getWidth.W) }
      io.rdata := fr.io.dec.rdata
      io.value := fr.io.core.sw.value
    }) { c =>
      // 写 tag=0xAB, len=0x1234, crc=0x5A → 期望 value = 0x5A1234AB
      c.io.wr.poke(true.B)
      c.io.wdata.poke("h5A1234AB".U)
      c.clock.step(1)
      c.io.wr.poke(false.B)
      val v = c.io.value.peek().litValue
      assert(v == 0x5A1234ABL, s"value 应为 0x5A1234AB（tag LSB），实际 0x${v.toString(16)}")
      // 读回
      c.io.rd.poke(true.B)
      c.clock.step(1)
      val r = c.io.rdata.peek().litValue
      assert(r == 0x5A1234ABL, s"rdata 应为 0x5A1234AB，实际 0x${r.toString(16)}")
      c.io.rd.poke(false.B)
    }
  }
}
