package HBS.tm

import BaseCbb.data.GenModule
import chisel3._





class PacketLinkList (val tmPar:TmParam) extends GenModule{
  val io = IO(new Bundle{
    val enq = Input(new PacketEnqInfo(tmPar))

  })
}