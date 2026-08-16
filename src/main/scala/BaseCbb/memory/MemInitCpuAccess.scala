package BaseCbb.memory

import chisel3._
import chisel3.util.{Enum, MuxLookup, log2Ceil, switch, is}

/**
 * MemInitCpuAccess — Common Building Block：存储器初始化 FSM + CPU(Rs) 访问仲裁 FSM。
 *
 * 从 SpMemoryWrap3 / TpMemoryWrap3 提取的公共逻辑（两处原实现逐行相同），
 * 统一为单一 CBB 供两个 Wrap3 复用，消除重复并保证行为一致。
 *
 * 功能：
 *  1. **初始化 FSM**（sIdle/sInit）：`init` 触发后逐地址写 0（全深度），完成后 `initDone` 置位；
 *  2. **CPU 访问 FSM**（sCpuIdle/sCpuWait/sCpuAccess/sCpuDone）：
 *     - CPU 请求在用户逻辑空闲（对应读/写空闲）时进入 sCpuAccess，`cpuAccessCnt` 计数至 `latency` 完成；
 *     - 用户忙时进 sCpuWait；等待超过 `RsMemoryDisLat` 超时 → ack + rdata=全 1 + status=3；
 *     - 完成时 `ack` 单拍脉冲，`status = (re && uerr) ? 1 : 0`，rdata = decData；
 *     - 超过 `idleCycleTh0` 等待时置 `cpuBackpress`；
 *     - 访问/完成期间 `blockUser` 屏蔽用户侧输入。
 *
 * 参数：
 *  - `dualPortAddr`：true = 双口（waddr/raddr 独立输出），false = 单口（addr 同时驱动两路）。
 *
 * 输出 `cpuMemStart` 为组合脉冲（空闲检测到的同一拍生效），供外层做输入 mux 优先级：
 * init > CPU-start > CPU-block > user。
 */
class MemInitCpuAccess(
  dataBits:        Int,
  addrWidth:       Int,
  latency:         Int,
  depth:           Int,
  RsAccess:        Boolean,
  RsMemoryDisLat:  Int,
  dualPortAddr:    Boolean = false
) extends Module {

  private val accCntW = log2Ceil(latency + 1)

  val io = IO(new Bundle {
    // ---- 输入 ----
    val init = Input(Bool())                    // 初始化触发
    val cpu  = new CpuRsPort(addrWidth, dataBits)
    val idleCycleTh0 = Input(UInt(16.W))        // backpressure 阈值
    // 用户侧读/写忙（供 CPU 仲裁判断空闲）
    val userReadActive  = Input(Bool())
    val userWriteActive = Input(Bool())
    // 完成拍采样（来自外层 ECC 解码）
    val decData = Input(UInt(dataBits.W))
    val uerr    = Input(Bool())

    // ---- 输出 ----
    val initDone    = Output(Bool())
    val initActive  = Output(Bool())
    val initWe      = Output(Bool())
    val initAddr    = Output(UInt(addrWidth.W))
    val initWdata   = Output(UInt(dataBits.W))
    // CPU 访问选通（组合，空闲检测同拍）
    val cpuMemStart = Output(Bool())
    // 屏蔽用户侧输入（CPU 访问/完成期间）
    val blockUser   = Output(Bool())
    val cpuWe       = Output(Bool())
    val cpuRe       = Output(Bool())
    val cpuWaddr    = Output(UInt(addrWidth.W))
    val cpuRaddr    = Output(UInt(addrWidth.W))
    val cpuWdataRaw = Output(UInt(dataBits.W))
    val cpuBackpress = Output(Bool())
  })

  // ════════════ Init FSM ════════════
  private val sIdle :: sInit :: Nil = Enum(2)
  private val state       = RegInit(sIdle)
  private val initCnt     = RegInit(0.U(addrWidth.W))
  private val initDoneReg = RegInit(false.B)

  private val initActive = state === sInit

  when(state === sIdle) {
    when(io.init) {
      initDoneReg := false.B
      state   := sInit
      initCnt := 0.U
    }
  }.elsewhen(state === sInit) {
    when(initCnt === (depth - 1).U) {
      state       := sIdle
      initDoneReg := true.B
    }.otherwise {
      initCnt := initCnt + 1.U
    }
  }

  io.initDone   := initDoneReg
  io.initActive := initActive
  io.initWe     := initActive
  io.initAddr   := initCnt
  io.initWdata  := 0.U(dataBits.W)

  // ════════════ CPU access FSM ════════════
  if (RsAccess) {
    // 读等待只看用户读，写等待只看用户写
    val cpuBlockedByUser = Mux(io.cpu.re, io.userReadActive,
                               Mux(io.cpu.we, io.userWriteActive, false.B))

    val sCpuIdle :: sCpuWait :: sCpuAccess :: sCpuDone :: Nil = Enum(4)
    val cpuState     = RegInit(sCpuIdle)
    val cpuWaitCnt   = RegInit(0.U(16.W))
    val cpuAccessCnt = RegInit(0.U(accCntW.W))
    val cpuRdataReg  = RegInit(0.U(dataBits.W))
    val cpuAckReg    = RegInit(false.B)
    val cpuStatusReg = RegInit(0.U(2.W))

    val cpuReq = io.cpu.re || io.cpu.we

    // cpuMemStart: 组合，空闲检测到的同一拍生效（空闲周期）
    val memStart = (cpuState === sCpuIdle && cpuReq && !cpuBlockedByUser) ||
                   (cpuState === sCpuWait && !cpuBlockedByUser)

    io.cpuBackpress := cpuState === sCpuWait && cpuWaitCnt >= io.idleCycleTh0

    cpuAckReg := false.B  // 默认单拍脉冲

    switch(cpuState) {
      is(sCpuIdle) {
        when(cpuReq) {
          when(!cpuBlockedByUser) {
            cpuState     := sCpuAccess
            cpuAccessCnt := 1.U
          }.otherwise {
            cpuState   := sCpuWait
            cpuWaitCnt := 0.U
          }
        }
      }
      is(sCpuWait) {
        cpuWaitCnt := cpuWaitCnt + 1.U
        when(cpuWaitCnt >= RsMemoryDisLat.U) {
          cpuState     := sCpuDone
          cpuRdataReg  := ~0.U(dataBits.W)
          cpuStatusReg := 3.U
          cpuAckReg    := true.B
        }.elsewhen(!cpuBlockedByUser) {
          cpuState     := sCpuAccess
          cpuAccessCnt := 1.U
        }
      }
      is(sCpuAccess) {
        cpuAccessCnt := cpuAccessCnt + 1.U
        when(cpuAccessCnt === latency.U) {
          cpuState     := sCpuDone
          cpuRdataReg  := io.decData
          cpuStatusReg := Mux(io.cpu.re && io.uerr, 1.U, 0.U)
          cpuAckReg    := true.B
        }
      }
      is(sCpuDone) {
        when(!cpuReq) {
          cpuState := sCpuIdle
        }
      }
    }

    io.cpu.rdata  := cpuRdataReg
    io.cpu.ack    := cpuAckReg
    io.cpu.status := cpuStatusReg

    io.cpuMemStart := memStart
    io.blockUser   := cpuState === sCpuAccess || cpuState === sCpuDone
    io.cpuWe       := io.cpu.we
    io.cpuRe       := io.cpu.re
    io.cpuWdataRaw := io.cpu.wdata
    io.cpuWaddr    := io.cpu.addr
    io.cpuRaddr    := io.cpu.addr
  } else {
    // RsAccess 关闭：CPU 输出 tie-off
    io.cpu.rdata      := 0.U
    io.cpu.ack        := false.B
    io.cpu.status     := 0.U
    io.cpuBackpress   := false.B
    io.cpuMemStart    := false.B
    io.blockUser      := false.B
    io.cpuWe          := false.B
    io.cpuRe          := false.B
    io.cpuWdataRaw    := 0.U(dataBits.W)
    io.cpuWaddr       := 0.U(addrWidth.W)
    io.cpuRaddr       := 0.U(addrWidth.W)
  }
}
