package BaseCbb.RegCbb.dsl.example

import chisel3._
import chisel3.util._
import BaseCbb.RegCbb.dsl._

/**
 * AXI4 Lite Interface Bundle
 * 支持burst的基础AXI接口（简化版，用于寄存器访问）
 */
class AxiLiteBusIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  // Write Address Channel
  val aw_valid = Input(Bool())
  val aw_ready = Output(Bool())
  val aw_addr  = Input(UInt(addrWidth.W))
  val aw_prot  = Input(UInt(3.W))

  // Write Data Channel
  val w_valid  = Input(Bool())
  val w_ready  = Output(Bool())
  val w_data   = Input(UInt(dataWidth.W))
  val w_strb   = Input(UInt((dataWidth/8).W))

  // Write Response Channel
  val b_valid  = Output(Bool())
  val b_ready  = Input(Bool())
  val b_resp   = Output(UInt(2.W))

  // Read Address Channel
  val ar_valid = Input(Bool())
  val ar_ready = Output(Bool())
  val ar_addr  = Input(UInt(addrWidth.W))
  val ar_prot  = Input(UInt(3.W))

  // Read Data Channel
  val r_valid  = Output(Bool())
  val r_ready  = Input(Bool())
  val r_data   = Output(UInt(dataWidth.W))
  val r_resp   = Output(UInt(2.W))
}

/**
 * AXI4 Full Interface Bundle (支持Burst)
 */
class AxiBusIO(addrWidth: Int, dataWidth: Int, idWidth: Int = 4) extends Bundle {
  // Write Address Channel
  val aw_valid = Input(Bool())
  val aw_ready = Output(Bool())
  val aw_id    = Input(UInt(idWidth.W))
  val aw_addr  = Input(UInt(addrWidth.W))
  val aw_len   = Input(UInt(8.W))
  val aw_size  = Input(UInt(3.W))
  val aw_burst = Input(UInt(2.W))
  val aw_lock  = Input(UInt(2.W))
  val aw_cache = Input(UInt(4.W))
  val aw_prot  = Input(UInt(3.W))
  val aw_qos   = Input(UInt(4.W))

  // Write Data Channel
  val w_valid  = Input(Bool())
  val w_ready  = Output(Bool())
  val w_id     = Input(UInt(idWidth.W))
  val w_data   = Input(UInt(dataWidth.W))
  val w_strb   = Input(UInt((dataWidth/8).W))
  val w_last   = Input(Bool())

  // Write Response Channel
  val b_valid  = Output(Bool())
  val b_ready  = Input(Bool())
  val b_id     = Output(UInt(idWidth.W))
  val b_resp   = Output(UInt(2.W))

  // Read Address Channel
  val ar_valid = Input(Bool())
  val ar_ready = Output(Bool())
  val ar_id    = Input(UInt(idWidth.W))
  val ar_addr  = Input(UInt(addrWidth.W))
  val ar_len   = Input(UInt(8.W))
  val ar_size  = Input(UInt(3.W))
  val ar_burst = Input(UInt(2.W))
  val ar_lock  = Input(UInt(2.W))
  val ar_cache = Input(UInt(4.W))
  val ar_prot  = Input(UInt(3.W))
  val ar_qos   = Input(UInt(4.W))

  // Read Data Channel
  val r_valid  = Output(Bool())
  val r_ready  = Input(Bool())
  val r_id     = Output(UInt(idWidth.W))
  val r_data   = Output(UInt(dataWidth.W))
  val r_resp   = Output(UInt(2.W))
  val r_last   = Output(Bool())
}

/** AXI Burst Type constants */
object AxiBurstType {
  val FIXED = "b00".U(2.W)
  val INCR  = "b01".U(2.W)
  val WRAP  = "b10".U(2.W)
}

/** AXI Response constants */
object AxiResp {
  val OKAY   = "b00".U(2.W)
  val EXOK   = "b01".U(2.W)
  val SLVERR = "b10".U(2.W)
  val DECERR = "b11".U(2.W)
}

/**
 * AXI4转寄存器块适配器
 * 将AXI请求转换为寄存器访问，支持burst操作
 */
class AxiToRegAdapter(
  val addrWidth: Int = 12,
  val dataWidth: Int = 32,
  val idWidth: Int = 4
) extends Module {
  val io = IO(new Bundle {
    val axi = new AxiBusIO(addrWidth, dataWidth, idWidth)
    val regAddr   = Output(UInt(addrWidth.W))
    val regWrEn   = Output(Bool())
    val regWrData = Output(UInt(dataWidth.W))
    val regRdEn   = Output(Bool())
    val regRdData = Input(UInt(dataWidth.W))
    val regWrAck  = Input(Bool())
    val regRdAck  = Input(Bool())
  })

  // FSM states for write transaction
  val wrIdle :: wrAddr :: wrData :: wrResp :: Nil = Enum(4)
  val wrState = RegInit(wrIdle)
  val wrAddrReg  = Reg(UInt(addrWidth.W))
  val wrLenReg   = Reg(UInt(8.W))
  val wrSizeReg  = Reg(UInt(3.W))
  val wrBurstReg = Reg(UInt(2.W))
  val wrCount    = Reg(UInt(8.W))
  val wrIdReg    = Reg(UInt(idWidth.W))

  // Write address channel handling
  when(wrState === wrIdle && io.axi.aw_valid) {
    wrAddrReg := io.axi.aw_addr
    wrLenReg  := io.axi.aw_len
    wrSizeReg := io.axi.aw_size
    wrBurstReg := io.axi.aw_burst
    wrIdReg := io.axi.aw_id
    wrCount := 0.U
    wrState := wrAddr
  }

  io.axi.aw_ready := wrState === wrIdle

  // Calculate beat address based on burst type
  val beatAddr = Wire(UInt(addrWidth.W))
  beatAddr := Mux(wrBurstReg === AxiBurstType.INCR,
    wrAddrReg + (wrCount << wrSizeReg),
    wrAddrReg)

  io.axi.w_ready := wrState === wrData
  io.regAddr := beatAddr
  io.regWrData := io.axi.w_data

  when(wrState === wrAddr && io.axi.aw_valid && io.axi.aw_ready) {
    wrState := wrData
  }

  io.regWrEn := false.B
  when(wrState === wrData && io.axi.w_valid) {
    io.regWrEn := true.B
    wrCount := wrCount + 1.U
    when(io.axi.w_last) {
      wrState := wrResp
    }
  }

  // Write response
  io.axi.b_valid := wrState === wrResp
  io.axi.b_id := wrIdReg
  io.axi.b_resp := Mux(io.regWrAck, AxiResp.OKAY, AxiResp.SLVERR)

  when(wrState === wrResp && io.axi.b_valid && io.axi.b_ready) {
    wrState := wrIdle
  }

  // FSM states for read transaction
  val rdIdle :: rdAddr :: rdData :: Nil = Enum(3)
  val rdState = RegInit(rdIdle)
  val rdAddrReg  = Reg(UInt(addrWidth.W))
  val rdLenReg   = Reg(UInt(8.W))
  val rdSizeReg  = Reg(UInt(3.W))
  val rdBurstReg = Reg(UInt(2.W))
  val rdCount    = Reg(UInt(8.W))
  val rdIdReg    = Reg(UInt(idWidth.W))

  val rdBeatAddr = Wire(UInt(addrWidth.W))
  rdBeatAddr := Mux(rdBurstReg === AxiBurstType.INCR,
    rdAddrReg + (rdCount << rdSizeReg),
    rdAddrReg)

  io.axi.ar_ready := rdState === rdIdle
  io.axi.r_valid := rdState === rdData
  io.axi.r_id := rdIdReg
  io.axi.r_data := io.regRdData
  io.axi.r_resp := Mux(io.regRdAck, AxiResp.OKAY, AxiResp.SLVERR)
  io.axi.r_last := (rdState === rdData) && (rdCount === rdLenReg)

  io.regAddr := rdBeatAddr

  when(rdState === rdIdle && io.axi.ar_valid) {
    rdAddrReg := io.axi.ar_addr
    rdLenReg  := io.axi.ar_len
    rdSizeReg := io.axi.ar_size
    rdBurstReg := io.axi.ar_burst
    rdIdReg := io.axi.ar_id
    rdCount := 0.U
    rdState := rdAddr
  }

  when(rdState === rdAddr && io.axi.ar_valid && io.axi.ar_ready) {
    rdState := rdData
  }

  io.regRdEn := rdState === rdData

  when(rdState === rdData && io.regRdAck) {
    rdCount := rdCount + 1.U
    when(rdCount === rdLenReg) {
      rdState := rdIdle
    }
  }
}

/**
 * 示例：带AXI4接口的寄存器块
 * 直接使用寄存器，不依赖RegisterFileGenerator
 */
class AxiRegBlockSample extends Module {
  val io = IO(new Bundle {
    val axi = new AxiBusIO(addrWidth = 13, dataWidth = 32, idWidth = 4)
    val irq = Output(Bool())
  })

  // 直接创建寄存器
  // ctrl @ 0x00: enable(1), mode(2), start(1) = 4 bits
  val ctrlReg = RegInit(0.U(32.W))
  // status @ 0x04: busy(1), done(1), error(1) = 3 bits
  val statusReg = RegInit(0.U(32.W))
  // data @ 0x08: value(32) = 32 bits
  val dataReg = RegInit(0.U(32.W))
  // irq_en @ 0x0C: en(1) = 1 bit
  val irqEnReg = RegInit(0.U(32.W))

  // 简化的AXI4 FSM状态
  val wrIdle :: wrAddr :: wrData :: wrResp :: Nil = Enum(4)
  val wrState = RegInit(wrIdle)
  val wrAddrReg = Reg(UInt(13.W))
  val wrLenReg = Reg(UInt(8.W))
  val wrCount = Reg(UInt(8.W))
  val wrIdReg = Reg(UInt(4.W))

  val rdIdle :: rdAddr :: rdData :: Nil = Enum(3)
  val rdState = RegInit(rdIdle)
  val rdAddrReg = Reg(UInt(13.W))
  val rdLenReg = Reg(UInt(8.W))
  val rdCount = Reg(UInt(8.W))
  val rdIdReg = Reg(UInt(4.W))

  // 寄存器地址解码 (基于0x100基地址)
  val regSelect = Wire(UInt(2.W))
  regSelect := Mux(wrState === wrIdle, io.axi.aw_addr(4, 2), rdAddrReg(4, 2))

  // AXI握手信号
  val awHandshake = io.axi.aw_valid && io.axi.aw_ready
  val wHandshake = io.axi.w_valid && io.axi.w_ready
  val bHandshake = io.axi.b_valid && io.axi.b_ready
  val arHandshake = io.axi.ar_valid && io.axi.ar_ready
  val rHandshake = io.axi.r_valid && io.axi.r_ready

  // Write address channel
  io.axi.aw_ready := wrState === wrIdle
  when(wrState === wrIdle && io.axi.aw_valid) {
    wrAddrReg := io.axi.aw_addr
    wrLenReg := io.axi.aw_len
    wrIdReg := io.axi.aw_id
    wrCount := 0.U
    wrState := wrAddr
  }

  // Write data channel
  io.axi.w_ready := wrState === wrData
  when(wrState === wrAddr && awHandshake) {
    wrState := wrData
  }

  // 寄存器写
  when(wrState === wrData && wHandshake) {
    switch(regSelect) {
      is(0.U) { ctrlReg := io.axi.w_data }
      is(2.U) { dataReg := io.axi.w_data }
      is(3.U) { irqEnReg := io.axi.w_data }
      // status是只读的，不写
    }
    wrCount := wrCount + 1.U
    when(io.axi.w_last) {
      wrState := wrResp
    }
  }

  // Write response
  io.axi.b_valid := wrState === wrResp
  io.axi.b_id := wrIdReg
  io.axi.b_resp := AxiResp.OKAY
  when(wrState === wrResp && bHandshake) {
    wrState := wrIdle
  }

  // Read address channel
  io.axi.ar_ready := rdState === rdIdle
  when(rdState === rdIdle && io.axi.ar_valid) {
    rdAddrReg := io.axi.ar_addr
    rdLenReg := io.axi.ar_len
    rdIdReg := io.axi.ar_id
    rdCount := 0.U
    rdState := rdAddr
  }

  // Read data channel
  io.axi.r_valid := rdState === rdData
  io.axi.r_id := rdIdReg
  io.axi.r_last := (rdState === rdData) && (rdCount === rdLenReg)

  // 寄存器读
  val rdRegSel = rdAddrReg(4, 2)
  io.axi.r_data := MuxCase(0.U, Seq(
    (rdRegSel === 0.U) -> ctrlReg,
    (rdRegSel === 1.U) -> statusReg,
    (rdRegSel === 2.U) -> dataReg,
    (rdRegSel === 3.U) -> irqEnReg
  ))
  io.axi.r_resp := AxiResp.OKAY

  when(rdState === rdAddr && arHandshake) {
    rdState := rdData
  }

  when(rdState === rdData) {
    rdCount := rdCount + 1.U
    when(rdCount === rdLenReg) {
      rdState := rdIdle
    }
  }

  // 功能逻辑示例
  val enable = ctrlReg(0)
  val mode = ctrlReg(2, 1)
  val start = ctrlReg(3)

  // 简单的状态机
  val busyReg = RegInit(false.B)
  val doneReg = RegInit(false.B)
  val errorReg = RegInit(false.B)

  when(!enable) {
    busyReg := false.B
    doneReg := false.B
    errorReg := false.B
  }.elsewhen(start) {
    busyReg := true.B
    doneReg := false.B
  }.elsewhen(busyReg && !errorReg) {
    busyReg := false.B
    doneReg := true.B
  }

  // 更新状态寄存器
  statusReg := Cat(0.U(29.W), errorReg, doneReg, busyReg)

  io.irq := irqEnReg(0) && doneReg
}
