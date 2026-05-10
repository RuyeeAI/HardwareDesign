package FPP.Parser

import BaseCbb.GenBundle
import chisel3._

class ethernet extends GenBundle{
  val da = UInt(48.W)
  val sa = UInt(48.W)
  val etherType = UInt(16.W)
}



