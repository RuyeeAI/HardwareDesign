package BaseCbb.RegCbb.hw

import chisel3._
import chisel3.util.RegEnable
import BaseCbb.RegCbb._

/** AXI4-Lite 从设备接口（方向为从设备视角：aw_valid 等为输入） */
class AxiLiteBusIO(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  // 写地址通道
  val aw_valid = Input(Bool())
  val aw_ready = Output(Bool())
  val aw_addr  = Input(UInt(addrWidth.W))
  val aw_prot  = Input(UInt(3.W))
  // 写数据通道
  val w_valid  = Input(Bool())
  val w_ready  = Output(Bool())
  val w_data   = Input(UInt(dataWidth.W))
  val w_strb   = Input(UInt((dataWidth / 8).W))
  // 写响应通道
  val b_valid  = Output(Bool())
  val b_ready  = Input(Bool())
  val b_resp   = Output(UInt(2.W))
  // 读地址通道
  val ar_valid = Input(Bool())
  val ar_ready = Output(Bool())
  val ar_addr  = Input(UInt(addrWidth.W))
  val ar_prot  = Input(UInt(3.W))
  // 读数据通道
  val r_valid  = Output(Bool())
  val r_ready  = Input(Bool())
  val r_data   = Output(UInt(dataWidth.W))
  val r_resp   = Output(UInt(2.W))
}

object AxiResp {
  val OKAY   = "b00".U(2.W)
  val EXOK   = "b01".U(2.W)
  val SLVERR = "b10".U(2.W)
  val DECERR = "b11".U(2.W)
}

/**
 * AXI4-Lite 寄存器文件包装器。
 *  - 单笔在途事务（aw 之后 w 到来；不支持流水）；
 *  - 读数据在 ar 握手拍寄存，r_valid 下一拍拉高（读返回 1 拍延迟）；
 *  - RO 寄存器经 io.user 正常驱动/读回（修复 v1 问题）。
 */
class AxiLiteRegFile(map: RegFileMap, addrWidth: Int = 32, dataWidth: Int = 32) extends Module {
  val io = IO(new Bundle {
    val axi  = new AxiLiteBusIO(addrWidth, dataWidth)
    val user = new RegUserRecord(map)
    val memPorts = new MemPortRecord(map.mems)
  })

  private val inner = Module(new RegFileTop(map, addrWidth, dataWidth))

  private val awHand = io.axi.aw_valid && io.axi.aw_ready
  private val wHand  = io.axi.w_valid  && io.axi.w_ready
  private val bHand  = io.axi.b_valid  && io.axi.b_ready
  private val arHand = io.axi.ar_valid && io.axi.ar_ready
  private val rHand  = io.axi.r_valid  && io.axi.r_ready

  private val wrAddrReg = RegEnable(io.axi.aw_addr, awHand)

  inner.io.wr := wHand
  inner.io.rd := arHand
  inner.io.addr := Mux(arHand, io.axi.ar_addr, wrAddrReg)
  inner.io.wdata := io.axi.w_data

  // 写响应
  private val bValid = RegInit(false.B)
  io.axi.b_valid := bValid
  io.axi.b_resp  := AxiResp.OKAY
  when(bValid && bHand) { bValid := false.B }
  when(wHand) { bValid := true.B }

  // 读数据：内层 rdata 为组合（寄存器）或请求-响应结果（memory，ack 拍更新），每拍锁存
  private val rValid = RegInit(false.B)
  private val rDataReg = RegNext(inner.io.rdata)
  io.axi.r_valid := rValid
  io.axi.r_data  := rDataReg
  io.axi.r_resp  := AxiResp.OKAY
  when(rValid && rHand) { rValid := false.B }
  when(arHand) { rValid := true.B }

  io.axi.aw_ready := true.B
  io.axi.w_ready  := true.B
  io.axi.ar_ready := true.B

  map.regs.foreach { a =>
    val name = a.reg.name
    CoreConnect(
      io.user.elements(name).asInstanceOf[RegCoreIO],
      inner.io.user.elements(name).asInstanceOf[RegCoreIO])
  }

  // 存储器接口透传（外部 SRAM 挂在包装器上）
  MemConnect(io.memPorts, inner.io.memPorts)
}
