package BaseCbb.utils

import chisel3._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountersSpec extends AnyFlatSpec with Matchers {

  "ZCounter" should "compile" in {
    assertCompiles("new ZCounter(10)")
  }

  "ZCounter" should "be usable in Chisel code" in {
    assertCompiles("""
      import chisel3._
      import chisel3.util._
      class ZCounterTest extends Module {
        val io = IO(new Bundle {
          val inc = Input(Bool())
          val value = Output(UInt(4.W))
        })
        val counter = new ZCounter(10)
        io.value := counter.value
        val wrap = Wire(Bool())
        when(io.inc) { wrap := counter.inc() }
      }
    """)
  }

  "TwoWayCounter" should "be usable in Chisel code" in {
    assertCompiles("""
      import chisel3._
      class TwoWayCounterTest extends Module {
        val io = IO(new Bundle {
          val up = Input(Bool())
          val down = Input(Bool())
          val count = Output(UInt(8.W))
        })
        io.count := TwoWayCounter(io.up, io.down, 16)
      }
    """)
  }

  "WideCounter" should "compile" in {
    assertCompiles("new WideCounter(16)")
  }
}