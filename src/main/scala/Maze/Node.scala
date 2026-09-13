package Maze

import BaseCbb.data.{GenBundle, GenModule}
import chisel3._
import chisel3.util.DecoupledIO

import scala.collection.mutable.ArrayBuffer

class PktDataBundle extends GenBundle {
  val data = UInt(8.W)
  val tx   = UInt(3.W)
  val ty   = UInt(3.W)
}



class Node (Pos:(Int,Int),Connection:ArrayBuffer[ArrayBuffer[ArrayBuffer[(Int,Int)]]] )extends GenModule{
  val Num = Connection.head.head.length
  val io = IO(new Bundle{
    val i_pkt = Flipped(DecoupledIO(new PktDataBundle()))
    val o_pkt = DecoupledIO(new PktDataBundle())
    val intfc = Flipped(new IntfC(Num))
  })

  io.intfc.idt.foreach(x=> x<>io.i_pkt)
  io.intfc.odt(1).ready := false.B
  io.intfc.odt(2).ready := false.B
  io.intfc.odt(3).ready := false.B
  io.o_pkt <> io.intfc.odt(0)

}
