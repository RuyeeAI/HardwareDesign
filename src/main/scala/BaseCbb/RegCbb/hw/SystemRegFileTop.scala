package BaseCbb.RegCbb.hw

import chisel3._
import chisel3.util.RegEnable
import chisel3.util.{MuxCase, log2Ceil}
import BaseCbb.RegCbb._

/**
 * 系统级寄存器文件顶层：
 *  - 一个系统包含多个功能模块（ModuleDef），每个模块内部含多个 RegBlock/MemBlock；
 *  - 模块基址由 AddressAllocator.allocateSystem 分配（自动或手工）；
 *  - 本模块做**两级译码分发**：顶层按模块基址译码 → 分发到各模块的 RegFileTop；
 *    读数据汇聚（MuxCase 按模块命中选择），未命中返回 0。
 *
 * 接口（简单总线，字节地址，32bit）：
 *  - io.wr / io.rd / io.addr / io.wdata / io.rdata：系统总线；
 *  - io.user：按寄存器名的用户连接面（覆盖全系统所有模块寄存器）；
 *  - io.memPorts：按存储器名的请求-响应端口（覆盖全系统所有模块存储器）。
 */
class SystemRegFileTop(sysMap: SystemMap, addrWidth: Int = 32, dataWidth: Int = 32) extends Module {
  require(dataWidth == 32, s"SystemRegFileTop 目前仅支持 32bit 总线，got $dataWidth")

  val io = IO(new Bundle {
    val wr       = Input(Bool())
    val rd       = Input(Bool())
    val addr     = Input(UInt(addrWidth.W))
    val wdata    = Input(UInt(dataWidth.W))
    val rdata    = Output(UInt(dataWidth.W))
    val user     = new RegUserRecord(sysMap.flatMap)
    val memPorts = new MemPortRecord(sysMap.allMemsAbsolute)
  })

  // ---------------- 每个功能模块一个 RegFileTop ----------------
  private val moduleTops: Seq[(ModuleAllocation, RegFileTop)] = sysMap.modules.map { ma =>
    val top = Module(new RegFileTop(ma.toRegFileMap, addrWidth, dataWidth))
    top.suggestName(s"module_${ma.module.name}")
    (ma, top)
  }

  // ---------------- 顶层译码：按模块基址命中 ----------------
  private val moduleHits: Seq[Bool] = moduleTops.map { case (ma, _) =>
    io.addr >= ma.baseAddress.U(addrWidth.W) &&
    io.addr < (ma.baseAddress + ma.sizeBytes).U(addrWidth.W)
  }

  // ---------------- 分发：wr/rd 只发给命中模块（未命中模块输入冻结） ----------------
  moduleTops.zip(moduleHits).foreach { case ((_, top), hit) =>
    top.io.wr    := io.wr && hit
    top.io.rd    := io.rd && hit
    top.io.addr  := io.addr
    top.io.wdata := io.wdata
  }

  // ---------------- 汇聚：读数据按模块命中选择 ----------------
  private val moduleRdata: Seq[UInt] = moduleTops.map(_._2.io.rdata)
  io.rdata := MuxCase(0.U(dataWidth.W), moduleHits.zip(moduleRdata))

  // ---------------- 用户连接面：全系统平铺透传 ----------------
  sysMap.allRegsAbsolute.foreach { ra =>
    val name = ra.reg.name
    val outer = io.user.elements(name).asInstanceOf[RegUserIO]
    val inner = moduleTops
      .find(_._1.allRegs.exists(_.reg.name == name))
      .map(_._2.io.user.elements(name).asInstanceOf[RegUserIO])
      .getOrElse(sys.error(s"register '$name' not found in any module"))
    CoreConnect(outer, inner)
  }

  // ---------------- 存储器接口：全系统平铺透传 ----------------
  sysMap.allMemsAbsolute.foreach { ma =>
    val name = ma.mem.name
    val outer = io.memPorts.elements(name).asInstanceOf[MemPortIO]
    val inner = moduleTops
      .find(_._1.allMems.exists(_.mem.name == name))
      .map(_._2.io.memPorts.elements(name).asInstanceOf[MemPortIO])
      .getOrElse(sys.error(s"memory '$name' not found in any module"))
    val op = outer
    val ip = inner
    op.rd    := ip.rd
    op.raddr := ip.raddr
    op.wr    := ip.wr
    op.waddr := ip.waddr
    op.wdata := ip.wdata
    ip.rdata := op.rdata
    ip.ack   := op.ack
    ip.status := op.status
  }
}

/** AXI4-Lite 系统级寄存器文件包装器（多模块译码分发） */
class SystemAxiLiteRegFile(sysMap: SystemMap, addrWidth: Int = 32, dataWidth: Int = 32) extends Module {
  val io = IO(new Bundle {
    val axi      = new AxiLiteBusIO(addrWidth, dataWidth)
    val user     = new RegUserRecord(sysMap.flatMap)
    val memPorts = new MemPortRecord(sysMap.allMemsAbsolute)
  })

  private val inner = Module(new SystemRegFileTop(sysMap, addrWidth, dataWidth))

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

  private val bValid = RegInit(false.B)
  io.axi.b_valid := bValid
  io.axi.b_resp  := AxiResp.OKAY
  when(bValid && bHand) { bValid := false.B }
  when(wHand) { bValid := true.B }

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

  sysMap.allRegsAbsolute.foreach { ra =>
    val name = ra.reg.name
    CoreConnect(
      io.user.elements(name).asInstanceOf[RegUserIO],
      inner.io.user.elements(name).asInstanceOf[RegUserIO])
  }

  sysMap.allMemsAbsolute.foreach { ma =>
    val name = ma.mem.name
    val op = io.memPorts.elements(name).asInstanceOf[MemPortIO]
    val ip = inner.io.memPorts.elements(name).asInstanceOf[MemPortIO]
    op.rd    := ip.rd
    op.raddr := ip.raddr
    op.wr    := ip.wr
    op.waddr := ip.waddr
    op.wdata := ip.wdata
    ip.rdata := op.rdata
    ip.ack   := op.ack
    ip.status := op.status
  }
}
