package BaseCbb.async

import chisel3._
import chisel3.experimental._

/**
 * 双端口同步触发器（BlackBox）。
 * 无复位端口，专用于跨时钟域同步链；综合时由后端单元库（如 lib_sync_ff）提供。
 */
class SYNC_FF extends BlackBox {
  val io = IO(new Bundle {
    val clk  = Input(Clock())
    val din  = Input(Bool())
    val dout = Output(Bool())
  })
}

/**
 * 多比特跨时钟域同步器。
 * 每比特使用 `StageNum` 级 SYNC_FF BlackBox 触发器链，将 `Width` 位数据
 * 从 `i_clk` 时钟域同步输出（仅适合控制类/握手信号，不保证多比特数据一致性）。
 */
class Sync(StageNum: Int = 2, Width: Int = 1) extends Module {
  require(StageNum >= 2, "Sync stage num must be >= 2")
  require(Width >= 1, "Sync width must be >= 1")

  val io = IO(new Bundle {
    val i_clk  = Input(Clock())
    val i_data = Input(UInt(Width.W))
    val o_data = Output(UInt(Width.W))
  })

  withClock(io.i_clk) {
    val chainOut = Wire(Vec(Width, Bool()))
    for (b <- 0 until Width) {
      val ffs = (0 until StageNum).map { _ => Module(new SYNC_FF) }
      ffs.foreach { ff => ff.io.clk := io.i_clk }
      ffs(0).io.din := io.i_data(b)
      for (i <- 1 until StageNum) {
        ffs(i).io.din := ffs(i - 1).io.dout
      }
      chainOut(b) := ffs(StageNum - 1).io.dout
    }
    io.o_data := chainOut.asUInt
  }
}
