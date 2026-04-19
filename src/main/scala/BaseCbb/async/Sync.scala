package BaseCbb.async
import chisel3._
import chisel3.util._

class SYNC_FF extends BlackBox{
  val io = IO(new Bundle{
    val CK = Input(UInt(1.W))
    val D  = Input(UInt(1.W))
    val Q  = Output(UInt(1.W))
  })
}

object SYNC_FF{
  def apply(CK:UInt,D:UInt) ={
    val U_SYNC = Module(new SYNC_FF).suggestName("U_SYNC_FF_INST")
    U_SYNC.io.D := D
    U_SYNC.io.CK := CK
    U_SYNC.io.Q
  }

}
class Sync (StageNum:Int=2, Width:Int = 1) extends Module {
  val io = IO(new Bundle {
    val i_data = Input(UInt(Width.W))
    val i_clk = Input(Clock())
    val o_data = Output(UInt(Width.W))
  })

  val data_ff = Wire(Vec(StageNum, UInt(Width.W)))
  for (i <- 0 until (StageNum)) {
    if (i == 0) data_ff(i) := Cat((0 until (Width)).map(x => SYNC_FF(io.i_clk.asUInt, io.i_data(x))))
    else data_ff(i) := Cat((0 until (Width)).map(x => SYNC_FF(io.i_clk.asUInt, data_ff(i - 1))))
  }
  io.o_data := data_ff(StageNum - 1)
}

object Sync{
  def apply(CLK:Clock,D:UInt,StageNum:Int=3)  = {
    val U_SYNC = Module(new Sync(StageNum,D.getWidth))
    U_SYNC.io.i_data := D
    U_SYNC.io.i_clk  := CLK
    U_SYNC.io.o_data
  }
}





