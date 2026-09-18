package em

import chisel3._
import chisel3.util._

// ===========================================================================
// CRC / 哈希
//
// 两种形态（由参数选择，见 CrcHardwired / CrcRuntime）：
//   固化  —— 多项式在 elaboration 期已知，展开成**平衡 XOR 树**：
//            组合逻辑、0 拍出结果、**无配置寄存器**，逻辑深度 ≈ log2(keyWidth)。
//   可配  —— 多项式/初值/输出异或放在寄存器里运行时改写，硬件是串行 LFSR：
//            keyWidth+1 拍出结果，面积小但慢。
//
// 算法形态统一为"左移 MSB 先"：poly 的 bit i 表示 x^i 的系数（x^width 隐含）。
// refin = true 时输入按 LSB 先喂入，refout = true 时输出按位反转，
// 组合起来即标准 reflected CRC（如以太网 CRC-32，check value 0xCBF43926）。
// ===========================================================================
object Crc {

  /** 平衡 XOR 树：n 个输入 → ceil(log2(n)) 级；空序列返回 false。 */
  private def xorTree(xs: Seq[Bool]): Bool = {
    if (xs.isEmpty) false.B
    else if (xs.size == 1) xs.head
    else {
      val (a, b) = xs.splitAt((xs.size + 1) / 2)
      xorTree(a) ^ xorTree(b)
    }
  }

  /** 串行推进一步（仅 elaboration 期用来算常向量，不进硬件）。 */
  private def stepBit(st: BigInt, bit: Int, width: Int, poly: BigInt): BigInt = {
    val fb: Int = ((st >> (width - 1)) & 1).toInt ^ bit
    var out = BigInt(0)
    var i = 0
    while (i < width) {
      val shifted: Int = if (i == 0) 0 else ((st >> (i - 1)) & 1).toInt
      val v: Int = shifted ^ (if (((poly >> i) & 1) == 1) fb else 0)
      if (v == 1) out |= (BigInt(1) << i)
      i += 1
    }
    out
  }

  private def runSerial(init: BigInt, bits: Seq[Int], width: Int, poly: BigInt): BigInt =
    bits.foldLeft(init) { (st, b) => stepBit(st, b, width, poly) }

  /**
   * 固化 CRC：展成**平衡 XOR 树**（不是逐位串行链）。
   *
   * 为什么可以这样做：CRC 的状态更新 `s' = A·s ⊕ b·v` 对输入位是**仿射**的，所以
   *   `终态 = A^n·init ⊕ Σ_p inBit(p)·A^(n-1-p)·v`
   * 即"每位输入有固定的 32 位贡献向量"，整个 CRC 就是一组输入的 XOR 组合。
   * 于是可以把原来的 `n 位 × 2 级 = 2n 级` 串行链换成 `⌈log2(n)⌉` 级的 XOR 树：
   *   keyWidth=48 时 96 级 → **6 级**（7nm 下 ~1.9ns → ~0.13ns），面积只多几组 XOR。
   * 数值与逐位串行实现**逐位一致**（由 CrcProbe 的等价性用例保证）。
   */
  def hardwired(
      data: UInt,
      width: Int,
      poly: BigInt,
      init: BigInt,
      xorout: BigInt,
      refin: Boolean,
      refout: Boolean
  ): UInt = {
    val n = data.getWidth
    require(n > 0, "Crc.hardwired: data 位宽必须 > 0")

    // 喂入顺序 → data 的位号
    val feedIdx: Seq[Int] = if (refin) (0 until n) else (0 until n).reverse

    // 常数项：所有输入位为 0 时的终态
    val constSt = runSerial(init, Seq.fill(n)(0), width, poly)

    // 每位输入对终态的贡献向量（= 只喂该位时的终态 XOR 全零时的终态）
    val contrib: Seq[BigInt] = feedIdx.map { p =>
      val basis = feedIdx.map(q => if (q == p) 1 else 0)
      runSerial(init, basis, width, poly) ^ constSt
    }

    // 每个终态位：把"贡献位为 1"的那些输入位 XOR 起来，再异或常数项
    val stBits: Seq[Bool] = (0 until width).map { j =>
      val ins = feedIdx.zip(contrib).collect { case (p, c) if ((c >> j) & 1) == 1 => data(p) }
      val x   = xorTree(ins)
      if (((constSt >> j) & 1) == 1) !x else x
    }

    val outBits = if (refout) (0 until width).map(i => stBits(width - 1 - i)) else stBits
    val mask    = (BigInt(1) << width) - 1
    VecInit(outBits).asUInt ^ (xorout & mask).U(width.W)
  }
}

/**
 * 运行时可配的串行 CRC（poly / init / xorout 均为输入，可随时改写）。
 *
 * 时序：拉高 start 并保持 din 稳定 → busy 拉高 → **dataWidth+1 拍后 done 拉高一拍、out 有效**。
 *
 * `refin` / `refout` 必须与 CrcHardwired 传同一套值，否则两种形态对同一个 key 会算出
 * **不同的哈希**（refin 决定喂位顺序：true = LSB 先，false = MSB 先；refout = 输出按位反转）。
 *
 * ⚠️ done 必须与 out 同拍有效：最后一次移位是在 `cnt==1` 那一拍写进 state 的，
 * 所以 done 要在**再下一拍**才拉高（用 fin 寄存器）。若在 `cnt==1` 当拍就报 done，
 * 消费方读到的 `state` 还差最后一位输入 —— 实测踩过。
 */
class CrcSerial(
    width: Int,
    dataWidth: Int,
    refin: Boolean = false,
    refout: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val din   = Input(UInt(dataWidth.W))
    val poly  = Input(UInt(width.W))
    val init  = Input(UInt(width.W))
    val xor   = Input(UInt(width.W))
    val busy  = Output(Bool())
    val done  = Output(Bool())
    val out   = Output(UInt(width.W))
  })

  require(dataWidth > 0)

  val state = RegInit(0.U(width.W))
  val shift = RegInit(0.U(dataWidth.W))
  val cnt   = RegInit(0.U(log2Ceil(dataWidth + 1).W))
  val run   = RegInit(false.B)
  val fin   = RegInit(false.B)     // 收尾拍：state 已是最终值，本拍报 done

  // refin：LSB 先喂（右移，取 bit0）；否则 MSB 先喂（左移，取 MSB）
  val bit  = if (refin) shift(0) else shift(dataWidth - 1)
  val fb   = state(width - 1) ^ bit
  val next = VecInit((0 until width).map { i =>
    val s = if (i == 0) false.B else state(i - 1)
    s ^ (io.poly(i) & fb)
  }).asUInt
  val shiftNext: UInt =
    if (refin) chisel3.util.Cat(0.U(1.W), shift(dataWidth - 1, 1))
    else if (dataWidth == 1) 0.U(1.W)
    else chisel3.util.Cat(shift(dataWidth - 2, 0), 0.U(1.W))
  // refout：输出按位反转（与 CrcHardwired 的 refout 语义一致，反转在 xorout 之前）
  val stOut: UInt =
    if (refout) VecInit((0 until width).map(i => state(width - 1 - i))).asUInt else state

  io.busy := run || fin
  io.done := fin
  io.out  := stOut ^ io.xor

  when(io.start && !run && !fin) {
    state := io.init
    shift := io.din
    cnt   := dataWidth.U
    run   := true.B
  }.elsewhen(run) {
    state := next
    shift := shiftNext
    cnt   := cnt - 1.U
    when(cnt === 1.U) {
      run := false.B
      fin := true.B            // 最后一次移位已在本拍写进 state，下一拍出 done
    }
  }.elsewhen(fin) {
    fin := false.B
  }
}
