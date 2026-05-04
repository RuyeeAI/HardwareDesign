package BaseCbb.RegCbb
import chisel3.DontCare.:=
import chisel3._
import BaseCbb._

/**
 * Register information
 */
case class RegInfo(
  regtype: String,      // "RW", "RO", "WO", "RC", "RS", "W1C", "W1S", "W1T"
  name: String,
  resetValue: BigInt,
  DataType: Data,
  DecAddr: Long = 0
)

/**
 * Register access types
 */
object RegAccessType {
  val RW = "RW"
  val RO = "RO"
  val WO = "WO"
  val RC = "RC"
  val RS = "RS"
  val W1C = "W1C"
  val W1S = "W1S"
  val W1T = "W1T"
}

/**
 * Core interface for RW register
 * - wrEn: pulse when software writes (for user logic to react)
 * - wrData: the value software wrote (for user logic to react)
 */
class rw_core_if(info: RegInfo) extends Bundle {
  val wrEn = Output(Bool())
  val wrData = Output(info.DataType)
}

/**
 * Core interface for WO register
 * - wrEn: pulse when software writes
 * - wrData: the value software wrote
 */
class wo_core_if(info: RegInfo) extends Bundle {
  val wrEn = Output(Bool())
  val wrData = Output(info.DataType)
}

/**
 * Core interface for RO register
 * - wrData: value driven by user logic (to be read by software)
 */
class ro_core_if(info: RegInfo) extends Bundle {
  val wrData = Flipped(Output(info.DataType))
}

/**
 * Core interface for RC register (Read-to-Clear)
 * - rdData: current register value
 * - rdEn: pulse when software reads (user logic should react to clear)
 */
class rc_core_if(info: RegInfo) extends Bundle {
  val rdData = Output(info.DataType)
  val rdEn = Output(Bool())
}

/**
 * Core interface for RS register (Read-to-Set)
 * - rdData: current register value
 * - rdEn: pulse when software reads (user logic should react to set)
 */
class rs_core_if(info: RegInfo) extends Bundle {
  val rdData = Output(info.DataType)
  val rdEn = Output(Bool())
}

/**
 * Core interface for W1C register (Write-1-to-Clear)
 * - wrEn: pulse when software writes
 * - wrData: the value software wrote (for w1c: which bits to clear)
 */
class w1c_core_if(info: RegInfo) extends Bundle {
  val wrEn = Output(Bool())
  val wrData = Output(info.DataType)
}

/**
 * Core interface for W1S register (Write-1-to-Set)
 * - wrEn: pulse when software writes
 * - wrData: the value software wrote (for w1s: which bits to set)
 */
class w1s_core_if(info: RegInfo) extends Bundle {
  val wrEn = Output(Bool())
  val wrData = Output(info.DataType)
}

/**
 * Core interface for W1T register (Write-1-to-Toggle)
 * - wrEn: pulse when software writes
 * - wrData: the value software wrote (for w1t: which bits to toggle)
 */
class w1t_core_if(info: RegInfo) extends Bundle {
  val wrEn = Output(Bool())
  val wrData = Output(info.DataType)
}

/**
 * Decoder interface (internal)
 */
class dec_in[T <: Data](gen: T) extends Bundle {
  val wr = Input(Bool())
  val wdata = Input(gen)
  val rd = Input(Bool())
}

class dec_out[T <: Data](gen: T) extends Bundle {
  val rdata = Output(gen)
}

class dec_if[T <: Data](gen: T = UInt(32.W)) extends Bundle {
  val in = new dec_in(gen)
  val out = new dec_out(gen)
}

/**
 * RW Register Module
 * - User logic reacts to writes via wrEn and wrData
 * - rdData is internal only (not exposed to user)
 */
class rw(info: RegInfo) extends Module {
  val io = IO(new Bundle {
    val dec = new dec_if(info.DataType)
    val core = new rw_core_if(info)
  })

  private val dataReg = RegInit(info.resetValue.U(info.DataType.getWidth.W))
  private val wrEnReg = RegNext(io.dec.in.wr, false.B)

  when(io.dec.in.wr) {
    dataReg := io.dec.in.wdata.asTypeOf(dataReg)
  }

  io.dec.out.rdata := dataReg
  io.core.wrEn := wrEnReg
  io.core.wrData := io.dec.in.wdata.asTypeOf(info.DataType)
}

/**
 * WO Register Module
 * - User logic reacts to writes via wrEn and wrData
 */
class wo(info: RegInfo) extends Module {
  val io = IO(new Bundle {
    val dec = new dec_if(info.DataType)
    val core = new wo_core_if(info)
  })

  private val dataReg = RegInit(info.resetValue.U(info.DataType.getWidth.W))
  private val wrEnReg = RegNext(io.dec.in.wr, false.B)

  when(io.dec.in.wr) {
    dataReg := io.dec.in.wdata.asTypeOf(dataReg)
  }

  io.dec.out.rdata := 0.U.asTypeOf(info.DataType)
  io.core.wrEn := wrEnReg
  io.core.wrData := io.dec.in.wdata.asTypeOf(info.DataType)
}

/**
 * RO Register Module
 * - User logic drives the value via core.wrData
 */
class ro(info: RegInfo) extends Module {
  val io = IO(new Bundle {
    val dec = Flipped(new dec_out(info.DataType))
    val core = new ro_core_if(info)
  })

  io.dec.rdata := io.core.wrData
}

/**
 * RC Register Module (Read-to-Clear)
 */
class rc(info: RegInfo) extends Module {
  val io = IO(new Bundle {
    val dec = new dec_if(info.DataType)
    val core = new rc_core_if(info)
  })

  private val dataReg = RegInit(info.resetValue.U(info.DataType.getWidth.W))
  private val rdEnReg = RegNext(io.dec.in.rd, false.B)

  when(io.dec.in.wr) {
    dataReg := io.dec.in.wdata.asTypeOf(dataReg)
  }
  when(io.dec.in.rd) {
    dataReg := 0.U.asTypeOf(dataReg)
  }

  io.dec.out.rdata := dataReg
  io.core.rdData := dataReg
  io.core.rdEn := rdEnReg
}

/**
 * RS Register Module (Read-to-Set)
 */
class rs(info: RegInfo) extends Module {
  val io = IO(new Bundle {
    val dec = new dec_if(info.DataType)
    val core = new rs_core_if(info)
  })

  private val dataReg = RegInit(info.resetValue.U(info.DataType.getWidth.W))
  private val rdEnReg = RegNext(io.dec.in.rd, false.B)

  when(io.dec.in.wr) {
    dataReg := io.dec.in.wdata.asTypeOf(dataReg)
  }
  when(io.dec.in.rd) {
    dataReg := ~0.U(info.DataType.getWidth.W).asTypeOf(dataReg)
  }

  io.dec.out.rdata := dataReg
  io.core.rdData := dataReg
  io.core.rdEn := rdEnReg
}

/**
 * W1C Register Module (Write-1-to-Clear)
 */
class w1c(info: RegInfo) extends Module {
  val io = IO(new Bundle {
    val dec = new dec_if(info.DataType)
    val core = new w1c_core_if(info)
  })

  private val dataReg = RegInit(info.resetValue.U(info.DataType.getWidth.W))
  private val wrEnReg = RegNext(io.dec.in.wr, false.B)

  when(io.dec.in.wr) {
    dataReg := dataReg & ~io.dec.in.wdata.asTypeOf(dataReg)
  }

  io.dec.out.rdata := dataReg
  io.core.wrEn := wrEnReg
  io.core.wrData := io.dec.in.wdata.asTypeOf(info.DataType)
}

/**
 * W1S Register Module (Write-1-to-Set)
 */
class w1s(info: RegInfo) extends Module {
  val io = IO(new Bundle {
    val dec = new dec_if(info.DataType)
    val core = new w1s_core_if(info)
  })

  private val dataReg = RegInit(info.resetValue.U(info.DataType.getWidth.W))
  private val wrEnReg = RegNext(io.dec.in.wr, false.B)

  when(io.dec.in.wr) {
    dataReg := dataReg | io.dec.in.wdata.asTypeOf(dataReg)
  }

  io.dec.out.rdata := dataReg
  io.core.wrEn := wrEnReg
  io.core.wrData := io.dec.in.wdata.asTypeOf(info.DataType)
}

/**
 * W1T Register Module (Write-1-to-Toggle)
 */
class w1t(info: RegInfo) extends Module {
  val io = IO(new Bundle {
    val dec = new dec_if(info.DataType)
    val core = new w1t_core_if(info)
  })

  private val dataReg = RegInit(info.resetValue.U(info.DataType.getWidth.W))
  private val wrEnReg = RegNext(io.dec.in.wr, false.B)

  when(io.dec.in.wr) {
    dataReg := dataReg ^ io.dec.in.wdata.asTypeOf(dataReg)
  }

  io.dec.out.rdata := dataReg
  io.core.wrEn := wrEnReg
  io.core.wrData := io.dec.in.wdata.asTypeOf(info.DataType)
}

