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

  override def cloneType: AxiLiteBusIO.this.type =
    new AxiLiteBusIO(addrWidth, dataWidth).asInstanceOf[this.type]
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

  override def cloneType: AxiBusIO.this.type =
    new AxiBusIO(addrWidth, dataWidth, idWidth).asInstanceOf[this.type]
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
    val axi = Flipped(new AxiBusIO(addrWidth, dataWidth, idWidth))
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
 */
class AxiRegBlockSample extends Module {
  val io = IO(new Bundle {
    val axi = new AxiBusIO(addrWidth = 12, dataWidth = 32, idWidth = 4)
    val irq = Output(Bool())
  })

  // 定义寄存器块
  val regBlock = RegBlock("sample") { b =>
    b.baseAddress(0x100)
    b.desc("Sample Register Block with AXI4 Interface")

    // 控制寄存器
    b.reg("ctrl") { r =>
      r.desc("Control register")
      r.field(RegField("enable", 1)(_.rw().reset(BigInt(0)).desc("Global enable")))
      r.field(RegField("mode", 2)(_.rw().reset(BigInt(0)).desc("Operation mode")))
      r.field(RegField("start", 1)(_.rw().reset(BigInt(0)).desc("Start signal")))
    }

    // 状态寄存器
    b.reg("status") { r =>
      r.desc("Status register")
      r.field(RegField("busy", 1)(_.ro().reset(BigInt(0)).desc("Busy flag")))
      r.field(RegField("done", 1)(_.ro().reset(BigInt(0)).desc("Done flag")))
      r.field(RegField("error", 1)(_.ro().reset(BigInt(0)).desc("Error flag")))
    }

    // 数据寄存器
    b.reg("data") { r =>
      r.desc("Data register")
      r.field(RegField("value", 32)(_.rw().reset(BigInt(0)).desc("Data value")))
    }

    // 64位数据寄存器（跨边界）
    b.reg("data64") { r =>
      r.desc("64-bit data register")
      r.field(RegField("low", 32)(_.rw().reset(BigInt(0)).desc("Low 32 bits")))
      r.field(RegField("high", 32)(_.rw().reset(BigInt(0)).desc("High 32 bits")))
    }

    // 中断使能寄存器
    b.reg("irq_en") { r =>
      r.desc("Interrupt enable register")
      r.field(RegField("en", 1)(_.rw().reset(BigInt(0)).desc("Interrupt enable")))
    }

    // Memory空间配置
    b.memBaseAddress(0x1000)
    b.mem("fifo") { m =>
      m.depth(128).dataWidth(32).sp().desc("Data FIFO")
    }
  }

  // 地址分配
  val map = AddressAllocator.allocate(regBlock)
  println(AddressAllocator.summarize(map))

  // 生成硬件
  val (regIO, regDataOut, memPorts) = RegisterFileGenerator.generate(map)

  // 连接AXI接口
  val axiAdapter = Module(new AxiToRegAdapter(12, 32, 4))
  axiAdapter.io.axi <> io.axi

  // 连接寄存器接口
  axiAdapter.io.regAddr   := regIO.reg_addr
  axiAdapter.io.regWrEn  := regIO.reg_wrEn
  axiAdapter.io.regWrData := regIO.reg_wrData
  axiAdapter.io.regRdEn   := regIO.reg_rdEn
  regIO.reg_rdData        := axiAdapter.io.regRdData
  axiAdapter.io.regWrAck := RegNext(regIO.reg_wrEn, false.B)
  axiAdapter.io.regRdAck := RegNext(regIO.reg_rdEn, false.B)

  // 功能逻辑示例
  val enable = regDataOut(0)(0)
  val mode   = regDataOut(0)(3, 2)
  val start  = regDataOut(0)(4)

  // 简单的状态机示例
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

  io.irq := regDataOut(4)(0) && doneReg

  // 连接Memory端口示例
}

/**
 * 简化的AXI Lite寄存器块示例
 */
class AxiLiteRegBlockSample extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AxiLiteBusIO(addrWidth = 12, dataWidth = 32))
  })

  // 定义寄存器块（简化版，无Memory）
  val regBlock = RegBlock("sample_lite") { b =>
    b.baseAddress(0x000)
    b.desc("Sample AXI Lite Register Block")

    b.reg("ctrl") { r =>
      r.desc("Control register")
      r.field(RegField("enable", 1)(_.rw().reset(BigInt(0)).desc("Enable")))
      r.field(RegField("mode", 2)(_.rw().reset(BigInt(0)).desc("Mode")))
      r.field(RegField("start", 1)(_.rw().reset(BigInt(0)).desc("Start")))
    }

    b.reg("status") { r =>
      r.desc("Status register")
      r.field(RegField("busy", 1)(_.ro().reset(BigInt(0)).desc("Busy")))
      r.field(RegField("done", 1)(_.ro().reset(BigInt(0)).desc("Done")))
    }

    b.reg("data") { r =>
      r.desc("Data register")
      r.field(RegField("value", 32)(_.rw().reset(BigInt(0)).desc("Data")))
    }
  }

  val map = AddressAllocator.allocate(regBlock)
  val (regIO, regDataOut, _) = RegisterFileGenerator.generate(map)

  // AXI Lite握手信号
  val awHandshake = io.axi.aw_valid && io.axi.aw_ready
  val wHandshake  = io.axi.w_valid && io.axi.w_ready
  val bHandshake  = io.axi.b_valid && io.axi.b_ready
  val arHandshake = io.axi.ar_valid && io.axi.ar_ready
  val rHandshake  = io.axi.r_valid && io.axi.r_ready

  // 地址寄存器
  val wrAddrReg = RegEnable(io.axi.aw_addr, awHandshake)
  val rdAddrReg = RegEnable(io.axi.ar_addr, arHandshake)

  // 寄存器选择
  val wrRegSel = wrAddrReg(11, 4)
  val rdRegSel = rdAddrReg(11, 4)

  // 写地址通道
  io.axi.aw_ready := true.B

  // 写数据通道
  io.axi.w_ready := true.B

  // 写响应通道
  val bValid = RegInit(false.B)
  io.axi.b_valid := bValid
  io.axi.b_resp := AxiResp.OKAY

  when(bValid && bHandshake) {
    bValid := false.B
  }
  when(wHandshake) {
    bValid := true.B
  }

  // 读地址通道
  io.axi.ar_ready := true.B

  // 读数据通道
  val rValid = RegInit(false.B)
  val rDataReg = Reg(UInt(32.W))

  io.axi.r_valid := rValid
  io.axi.r_data := rDataReg
  io.axi.r_resp := AxiResp.OKAY

  when(rValid && rHandshake) {
    rValid := false.B
  }

  // 寄存器写
  val wrEn = awHandshake && wHandshake
  regIO.reg_addr := Mux(awHandshake, io.axi.aw_addr, io.axi.ar_addr)
  regIO.reg_wrEn := wrEn
  regIO.reg_wrData := io.axi.w_data
  regIO.reg_rdEn := arHandshake
  regIO.reg_rdData := 0.U

  // 寄存器读
  when(arHandshake) {
    rValid := true.B
    switch(rdRegSel) {
      is(0.U) { rDataReg := Cat(0.U(29.W), 1.U, 2.U(2.W), 0.U) }
      is(1.U) { rDataReg := Cat(0.U(30.W), 0.U, 1.U) }
      is(2.U) { rDataReg := 0.U }
    }
  }
}
