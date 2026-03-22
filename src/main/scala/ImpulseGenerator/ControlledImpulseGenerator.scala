package ImpulseGenerator

import chisel3._
import chisel3.util._

/**
 * 可控序列脉冲发生器
 * 
 * 输入：
 *   - clk: 时钟信号
 *   - reset: 复位信号（高电平有效）
 *   - start: 启动信号，触发脉冲生成
 *   - length: 4位，控制输出脉冲的个数（0~15，实际输出 length+1 个脉冲）
 *   - width: 2位，控制每个脉冲的宽度（时钟周期数）
 *             00 = 1个周期
 *             01 = 2个周期
 *             10 = 3个周期
 *             11 = 4个周期
 * 输出：
 *   - out: 脉冲输出
 */
class ControlledImpulseGenerator extends Module {
  val io = IO(new Bundle {
    val start  = Input(Bool())
    val length = Input(UInt(4.W))
    val width  = Input(UInt(2.W))
    val out    = Output(Bool())
  })

  // 状态定义：空闲、生成脉冲、保持低电平
  val sIdle :: sGenerate :: sWait :: Nil = Enum(3)
  val state = RegInit(sIdle)

  // 计算实际脉冲宽度和间隔（脉冲之间间隔等于宽度）
  val actualWidth = io.width + 1.U
  // 脉冲计数器，记录已经输出了多少个脉冲
  val pulseCount = Reg(UInt(4.W))
  // 周期计数器，记录当前脉冲已经持续了多少个周期
  val cycleCount = Reg(UInt(3.W))

  // 默认输出低电平
  io.out := false.B

  switch(state) {
    is(sIdle) {
      when(io.start) {
        state := sGenerate
        pulseCount := 0.U
        cycleCount := 1.U
      }
    }

    is(sGenerate) {
      // 输出高电平，脉冲有效
      io.out := true.B
      
      when(cycleCount < actualWidth) {
        // 继续保持高电平
        cycleCount := cycleCount + 1.U
      }.otherwise {
        // 脉冲宽度到了，结束这个脉冲，进入等待间隔
        cycleCount := 1.U
        pulseCount := pulseCount + 1.U
        when(pulseCount === io.length) {
          // 所有脉冲都发完了，回到空闲
          state := sIdle
        }.otherwise {
          // 还有脉冲要发，进入等待间隔
          state := sWait
        }
      }
    }

    is(sWait) {
      // 等待间隔和脉冲宽度相同
      when(cycleCount < actualWidth) {
        // 继续等待
        cycleCount := cycleCount + 1.U
      }.otherwise {
        // 等待结束，开始下一个脉冲
        cycleCount := 1.U
        state := sGenerate
      }
    }
  }

  // 复位处理
  when(reset) {
    state := sIdle
    pulseCount := 0.U
    cycleCount := 0.U
  }
}
