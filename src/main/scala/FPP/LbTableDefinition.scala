package FPP

import BaseCbb.data.{GenBundle, fldAttr}
import chisel3._
import chisel3.util.log2Ceil

class EcmpGroupTblEntry(NhpDep:Int,MemberNum:Int) extends GenBundle{
  val valid = Bool()
  Attr +=(valid->fldAttr(Desc = "Valid Flag"))

  val lb_mode = UInt(3.W)
  Attr +=(lb_mode -> fldAttr(Desc = "0:Static Mode;\n" +
    "1:Static Extend Load Balance\n" +
    "2:Random LB\n" +
    "3:RR LB\n" +
    "4:DLB"))

  val base_addr = UInt(log2Ceil(NhpDep).W)
  Attr +=(base_addr -> fldAttr(Desc = "The base address of NHP member table"))

  val num_all = UInt(log2Ceil(MemberNum).W)
  Attr +=(num_all->fldAttr(Desc = "The member all number of Member table"))

  val wit_field = UInt(19.W)
  Attr +=(wit_field->fldAttr(Desc = "Bit[18]"))
}
