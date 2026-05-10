package BaseCbb.utils

import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MuxLiteralSpec extends AnyFlatSpec with Matchers {

  "MuxLiteral" should "create lookup table with literal keys" in {
    assertCompiles("""
      import chisel3._
      import BaseCbb.utils.MuxLiteral
      val index = Wire(UInt(4.W))
      val result = MuxLiteral(index, 0.U, (1.U, 10.U), (3.U, 30.U), (5.U, 50.U))
    """)
  }

  "MuxSeq" should "create sequential mux" in {
    assertCompiles("""
      import chisel3._
      import BaseCbb.utils.MuxSeq
      val index = Wire(UInt(3.W))
      val result = MuxSeq(index, 0.U, 10.U, 20.U, 30.U)
    """)
  }

  "MuxTable" should "compile with dense encoding" in {
    assertCompiles("""
      import chisel3._
      import BaseCbb.utils.MuxTable
      val index = Wire(UInt(4.W))
      val result = MuxTable(index, 0.U, (scala.BigInt(1), 10.U), (scala.BigInt(2), 20.U))
    """)
  }
}