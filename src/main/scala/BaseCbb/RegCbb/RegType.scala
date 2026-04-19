package BaseCbb.RegCbb
import chisel3.DontCare.:=
import chisel3._
import chisel3.util.RegEnable
import chisel3.util.experimental.BoringUtils
import BaseCbb._

case class RegInfo(
                  regtype:String,
                  name :String,
                  resetValue:Long,
                  DataType:Data,
                  DecAddr:Long
                  )

class rw_if (info:RegInfo) extends GenBundle{
  val data   = Input(info.DataType)
  val update = Input(Bool())
}

class wo_if (info:RegInfo) extends GenBundle{
  val data   = Input(info.DataType)
  val update = Input(Bool())
}

class ro_if (info:RegInfo) extends GenBundle{
  val data   = Output(info.DataType)
}

class rwc_if(info:RegInfo) extends GenBundle{
//TODO
}

class dec_if_in[T<:Data] (gen:T) extends GenBundle{
  val wr    = Input(Bool())
  val wdata = Input(gen)
  val rd    = Input(Bool())
}

class dec_if[T<:Data](gen:T=UInt(32.W)) extends GenBundle{
  val in = new dec_if_in(gen)
  val out = Output(UInt(32.W))
}

class rw [T<:Data] (info:RegInfo) extends Module{
  val io = IO(new Bundle{
    val dec_if  = new dec_if
    val core_if = Flipped(new rw_if(info))
  })

  val data = RegInit(info.resetValue.U)
  when(io.dec_if.in.wr){
    data := io.dec_if.in.wdata
  }
  io.dec_if.out := data
  io.core_if.update := io.dec_if.in.wr
  io.core_if.data   := data
}

object rw{
  def apply(info:RegInfo,dec_if_in:dec_if_in[Data])={
    val reg = Module(new rw(info)).suggestName(info.name)
    reg.io.dec_if.in := dec_if_in
    (reg.io.dec_if.out,reg.io.core_if.data,reg.io.core_if.update)
  }
}

class wo [T<:Data] (info:RegInfo) extends Module{
  val io = IO(new Bundle{
    val dec_if  = new dec_if
    val core_if = Flipped(new wo_if(info))
  })
  io.core_if.update := io.dec_if.in.wr
  io.core_if.data   := io.dec_if.in.wdata
  io.dec_if.out     := 0.U
}


class ro [T<:Data] (info:RegInfo) extends Module{
  val io = IO(new Bundle{
    val dec_if  = new dec_if
    val core_if = Output(new ro_if(info))
  })
  io.dec_if.out := io.core_if.data
}





class RegDecGroup (RegList:Seq[RegInfo]) extends Module{
  val io = IO(new Bundle {
    val wr    = Input(Bool())
    val rd    = Input(Bool())
    val addr  = Input(UInt(16.W))
    val wdata = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
  })

  private val rdata_vec = Wire(Vec(RegList.length,UInt(32.W)))
  private val dec_rd    = Wire(Vec(RegList.length,Bool()))
  io.rdata := RegEnable(rdata_vec.reduceTree(_|_),io.rd)

  for(i<- RegList.indices){
    val r = RegList(i)
    dec_rd(i) := io.rd && (io.addr === r.DecAddr.U)
    if(r.regtype=="RW"){
      val reg = Module(new rw(r)).suggestName(r.name)
      reg.io.dec_if.in.wr := io.wr
      reg.io.dec_if.in.rd := io.rd
      reg.io.dec_if.in.wdata := io.wdata.asTypeOf(r.DataType)
      rdata_vec(i) := Mux(dec_rd(i),reg.io.dec_if.out,0.U)
    }else if (r.regtype=="WO"){
    }
  }

  io.rdata := rdata_vec.reduceTree(_|_)
}


