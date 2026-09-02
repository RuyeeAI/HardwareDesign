package BaseCbb.align

import BaseCbb.data.GenModule
import BaseCbb.Area.ProcessConfiguration
import chisel3._
import chisel3.util.{Cat, log2Ceil, UIntToOH}

/** 从 N bit 输入中按 G 粒度移位、取出 M bit 的模块。
  *
  * 偏移起点为 s·G，s ∈ [0, S)，S = (N−M)/G + 1。约束：N、M 均为 G 的整数倍
  * （N = n·G，M = m·G），因此问题等价于「从 n 个 G-bit chunk 中选 m 个连续 chunk」。
  *
  * 本文件提供三种实现：
  *  - [[GranularExtractTree]]   窗口化二分块树（B）：K 级、只对收缩到 M 的窗口移位，面积 Θ(M·log S + N)。
  *  - [[GranularExtractBitmap]] bitmap AND-OR 平面（T2）：每输出位一个 S:1 独热 mux，面积 Θ(M·S)。
  *  - [[GranularExtractAuto]]   按参数估计两种实现的面积（等效 2:1 mux 数），自动选更小者实例化。
  *
  * 两种实现输出完全一致（均为 out = in >> (off·G) 的低 M bit），可由同一黄金模型验证。
  * 另提供 per-chunk 的 sideband（valid/last 等 1 位/chunk 标志）：sideIn 宽 n、sideOut 宽 m，
  * 走同一套按 chunk 移位的网络（等价于 GranularExtract(n, m, G=1)）。
  *
  * @param N      输入位宽
  * @param M      输出位宽（M ≤ N）
  * @param G      粒度（偏移步长），要求 G | N 且 G | M
  * @param prefer 实现偏好：auto（默认，选面积更小）/ tree（强制二分块树）/ bitmap（强制 AND-OR 平面）
  */
class GranularExtractAuto(
    val N: Int,
    val M: Int,
    val G: Int,
    val prefer: String = "auto"
) extends GenModule {

  require(N >= M && G >= 1 && N % G == 0 && M % G == 0,
    s"GranularExtractAuto: need N>=M, G|N, G|M (got N=$N, M=$M, G=$G)")
  require(Seq("auto", "tree", "bitmap").contains(prefer),
    s"prefer must be auto|tree|bitmap, got $prefer")

  val n: Int = N / G
  val m: Int = M / G
  val S: Int = (N - M) / G + 1 // 可选窗口数
  val K: Int = log2Ceil(S)     // 桶形级数 / 偏移位宽

  // ---- 面积估计（等效 2:1 mux 数，工艺无关，用于决策）----
  // B 二分块树：K 级、每级对收缩窗口做 2:1 mux，窗口宽之和 = M + G·(2^K−1−K)
  val mux2Tree: Long = (K * M + G * ((1L << K) - 1 - K)).toLong
  // T2 bitmap AND-OR 平面：每输出位一个 S:1 独热 mux（≈ S−1 个 mux2），加 one-hot 译码 S·K
  val mux2Bitmap: Long = (M * (S - 1) + S * K).toLong

  // 自动决策：选等效 mux2 更少者；prefer 可强制覆盖
  val useTree: Boolean = prefer match {
    case "tree"   => true
    case "bitmap" => false
    case "auto"   => mux2Tree <= mux2Bitmap
  }

  val io = IO(new Bundle {
    val in      = Input(UInt(N.W))
    val off     = Input(UInt(K.W)) // G 粒度偏移，调用方需保证 off < S
    val out     = Output(UInt(M.W))
    val sideIn  = Input(UInt(n.W))  // per-chunk 1 位标志（n 个输入 chunk）
    val sideOut = Output(UInt(m.W)) // 选中的 m 个输出 chunk 标志
  })

  // 数据通路：按决策实例化对应的叶子实现（公共基类 GranularExtractLeaf 统一 io 类型）
  private val impl: GranularExtractLeaf =
    if (useTree) Module(new GranularExtractTree(N, M, G))
    else Module(new GranularExtractBitmap(N, M, G))
  impl.io.in := io.in
  impl.io.off := io.off
  io.out := impl.io.out

  // sideband：等价于 GranularExtract(n, m, G=1)，复用同一决策（S 相同，K 也相同）
  private val sbImpl: GranularExtractLeaf =
    if (useTree) Module(new GranularExtractTree(n, m, 1))
    else Module(new GranularExtractBitmap(n, m, 1))
  sbImpl.io.in := io.sideIn
  sbImpl.io.off := io.off
  io.sideOut := sbImpl.io.out

  // ---- 诊断信息（elaboration 日志 / 文档引用）----
  def chosenImpl: String = if (useTree) "tree(B)" else "bitmap(T2)"
  def areaUm2Tree: Double = mux2Tree * ProcessConfiguration.pd_mux2_area
  def areaUm2Bitmap: Double = mux2Bitmap * ProcessConfiguration.pd_mux2_area
  def savingPct: Double =
    if (useTree) (1.0 - mux2Tree.toDouble / mux2Bitmap) * 100
    else (1.0 - mux2Bitmap.toDouble / mux2Tree) * 100
}

/** 两种叶子实现的公共基类：统一 IO 类型，供自动选择模块用单一 val 持有。 */
abstract class GranularExtractLeaf(N: Int, M: Int, G: Int) extends GenModule {
  require(N >= M && G >= 1 && N % G == 0 && M % G == 0,
    s"GranularExtractLeaf: need N>=M, G|N, G|M (got N=$N, M=$M, G=$G)")
  val n: Int = N / G
  val m: Int = M / G
  val S: Int = (N - M) / G + 1
  val K: Int = log2Ceil(S)
  val io = IO(new Bundle {
    val in  = Input(UInt(N.W))
    val off = Input(UInt(K.W))
    val out = Output(UInt(M.W))
  })
}

/** 窗口化二分块树（方案 B）。
  *
  * 偏移按二进制分解 off = Σ off[k]·2^k；off[k]=1 表示窗口相对起点再「下移」2^k·G 位
  * （即 start 累加 2^k·G，out[j] = in[start + j]）。每级只做「保持 / 下移 2^k·G」的
  * 2:1 mux，窗口从 W0 = M + (2^K−1)·G 逐级收缩到 M。选通就是 off 的二进制位，无需译码；
  * 每级可插寄存器流水（延迟 +K 拍，吞吐不变）。
  *
  * 不变式：进入第 k 级时 win 宽为 Wk = M + (2^(k+1)−1)·G，win[j] = in[acc + j]
  * （acc 为已累加的 start 部分）；off[k]=0 取 win 低窗口 keep=win(Wk−sh−1,0)，
  * off[k]=1 取 fwd=win(Wk−1, sh)（win 整体下移 sh 后的低窗口）。
  */
class GranularExtractTree(N: Int, M: Int, G: Int) extends GranularExtractLeaf(N, M, G) {
  private val W0 = (M + ((1L << K) - 1) * G).toInt // 顶层窗口宽（≥ N，多出部分补 0 不耗面积）
  private val win0 = Cat(0.U(((W0 - N).max(0)).W), io.in) // W0 位，高位补 0（off<S 时永不选中）
  private var win = win0
  for (k <- K - 1 to 0 by -1) {
    val Wk = M + ((1 << (k + 1)) - 1) * G // 本级输入窗口宽
    val sh = (1 << k) * G                 // 本级下移量
    val keep = win(Wk - 1 - sh, 0)        // 低窗口（off[k]=0），宽 Wk' = Wk − sh
    val fwd = win(Wk - 1, sh)             // 下移 sh 后的低窗口（off[k]=1），宽 Wk'
    win = Mux(io.off(k), fwd, keep)       // 选通为 off 的二进制位
  }
  io.out := win(M - 1, 0) // 末级窗口宽 = M + G，取低 M 位
}

/** bitmap AND-OR 平面（方案 T2，数据保真）。
  *
//  * offset 译码为 S 路 one-hot sel；每个输出位 = Σ_s (sel_s ∧ in[位 s]) 的 OR，
  * 即一个 S:1 独热 mux。结果等价于「先 mask 再 OR 折叠」——可保真提取，而非有损 AND 折叠。
  * 当 c+s ≥ n（非法窗口）时该项为 0，属 don't-care，不耗面积。
  */
class GranularExtractBitmap(N: Int, M: Int, G: Int) extends GranularExtractLeaf(N, M, G) {
  private val sel = UIntToOH(io.off, S).asBools // S 个译码位
  private val outBits = Wire(Vec(M, Bool()))
  for (c <- 0 until m) {
    for (b <- 0 until G) {
      val terms = (0 until S).map { s =>
        val src = if (c + s < n) io.in((c + s) * G + b) else false.B
        src && sel(s) // 独热下恰一项有效 → 等价 mux
      }
      outBits(c * G + b) := terms.reduce(_ || _)
    }
  }
  io.out := outBits.asUInt
}
