package P4C

/** 延迟模型（对标 XLS `--delay_model`）：为调度器提供节点的逻辑延迟代价。
  *
  * 口径：以 **ND2（二输入 NAND）的门延迟为归一化单位**——ND2 一级 = 1.0（X7，
  * Logic Effort 方式）；0 = 纯布线。调度器据此计算加权深度 arrival(x) 并分桶；
  * clock 模式（每级组合延迟上限）同样以该口径度量。工艺特征化模型实现本 trait 即可接入。
  */
trait DelayModel {
  /** 模型名（日志/签名用）。 */
  def name: String
  /** 节点延迟代价（ND2 级数倍数，可为小数；0 = 纯布线）。 */
  def weight(n: Ir.Node): Double
}

object DelayModels {

  /** E1 加权表（默认，与历史行为逐字节一致；值恰为整数，Double 表示精确）。 */
  object Weighted extends DelayModel {
    val name: String = "weighted"
    def weight(n: Ir.Node): Double = n match {
      case _: Ir.Const | _: Ir.InputRef | _: Ir.Cat | _: Ir.Slice |
           _: Ir.Zext | _: Ir.Trunc | _: Ir.Not => 0.0
      case _: Ir.Bin | _: Ir.Mux => 1.0
      case _: Ir.RegRead => 2.0
    }
  }

  /** XLS unit 模型对标：叶子（Const/InputRef）为 0，其余每节点 1。 */
  object Unit extends DelayModel {
    val name: String = "unit"
    def weight(n: Ir.Node): Double = n match {
      case _: Ir.Const | _: Ir.InputRef => 0.0
      case _ => 1.0
    }
  }

  /** X7：Logic Effort 模型（Sutherland/Sproull/Harris），ND2 归一化。
    *
    * 单门 op：d = (g·h + p) / (g_ND2·h + p_ND2)，参考扇出 h = 1，其中
    * ND2 = g 4/3 + p 2 = 10/3 τ（INV = g 1 + p 1 = 2τ → 0.6；
    * NAND+INV（And/Or）= 1.6；XOR2 = g 4 + p 6 → 3.0；2:1 mux = g 2 + p 2 → 1.2）。
    *
    * 复合 op 按门网络级数展开（宽度 w 相关）：
    *   - Add/Sub：行波进位链上界，每 bit ≈ 1 ND2 → w（综合可能构建 CLA 更快，
    *     高估方向保守——切更多级只会更慢，不会违时序）；
    *   - Shl/Shr：桶形移位 = log2(w) 级 2:1 mux；
    *   - Eq/Neq：按位 XNOR（并行）+ AND 归约树；
    *   - Lt/Le/Gt/Ge：树形比较器近似。
    *
    * 已知简化：忽略扇出负载（g·h 的 h 取 1）与线电容；真实时序以综合 + STA 为准。
    */
  object LogicalEffort extends DelayModel {
    val name: String = "logiceffort"

    private def log2ceil(x: Int): Int = math.max(0, BigInt(math.max(0, x - 1)).bitLength)

    def weight(n: Ir.Node): Double = n match {
      case _: Ir.Const | _: Ir.InputRef | _: Ir.Cat | _: Ir.Slice |
           _: Ir.Zext | _: Ir.Trunc => 0.0 // 纯布线
      case _: Ir.Not => 0.6 // INV
      case _: Ir.Mux => 2 // 2:1 mux  Note:change by haoyu @20260906 from 1.2->2
      case Ir.Bin(op, _, _, w) => op match {
        case Ir.And | Ir.Or => 1.6 // NAND/NOR + INV
        case Ir.Xor => 3.0 // XOR2（按位并行，与 w 无关）
        case Ir.Add | Ir.Sub => w.toDouble // 行波进位链上界
        case Ir.Shl | Ir.Shr => 1.2 * log2ceil(w) // 桶形移位
        case Ir.Eq | Ir.Neq => 3.0 + 1.6 * log2ceil(w) // XNOR + AND 树
        case Ir.Lt | Ir.Le | Ir.Gt | Ir.Ge => 3.0 + 2.4 * log2ceil(w) // 树形比较器
      }
      case Ir.RegRead(_, _, _, size) =>
        math.max(1.0, 1.2 * log2ceil(size)) // 读 mux 树：log2(size) 级 2:1 mux
    }
  }

  val builtin: Map[String, DelayModel] =
    Map(Weighted.name -> Weighted, Unit.name -> Unit, LogicalEffort.name -> LogicalEffort)

  val default: DelayModel = Weighted

  private val requiredOps = Seq("Const", "InputRef", "Cat", "Slice", "Zext", "Trunc", "Not", "Bin", "Mux", "RegRead")

  /** 解析模型规格：内置名（weighted/unit/logiceffort，大小写不敏感）或 JSON 文件路径。
    *
    * JSON 形如 `{"Const":0,"InputRef":0,...,"Bin":1,"Mux":1.2,"RegRead":2}`（允许小数，
    * 即 ND2 倍数口径）；可用 `"Bin(Add)": 12` 按运算符细分（未细分的运算符回落到 `"Bin"`）。
    * 缺少任一必需项 → [[P4Error]]（带模型路径与缺失项清单）。
    */
  def load(spec: String): DelayModel =
    builtin.get(spec.toLowerCase).getOrElse {
      val path = java.nio.file.Paths.get(spec)
      val txt = try new String(java.nio.file.Files.readAllBytes(path), java.nio.charset.StandardCharsets.UTF_8)
      catch { case e: java.io.IOException => throw new P4Error(s"无法读取延迟模型文件 '$spec'：${e.getMessage}") }
      val entries = """"(\w+(?:\(\w+\))?)"\s*:\s*(\d+(?:\.\d+)?)""".r
        .findAllMatchIn(txt).map(m => m.group(1) -> m.group(2).toDouble).toMap
      val missing = requiredOps.filterNot(entries.contains)
      if (missing.nonEmpty)
        throw new P4Error(s"延迟模型文件 '$spec' 缺少权重项：${missing.mkString(", ")}")
      new DelayModel {
        val name: String = spec
        def weight(n: Ir.Node): Double = n match {
          case Ir.Bin(op, _, _, _) => entries.getOrElse(s"Bin($op)", entries("Bin"))
          case _: Ir.Const => entries("Const")
          case _: Ir.InputRef => entries("InputRef")
          case _: Ir.Cat => entries("Cat")
          case _: Ir.Slice => entries("Slice")
          case _: Ir.Zext => entries("Zext")
          case _: Ir.Trunc => entries("Trunc")
          case _: Ir.Not => entries("Not")
          case _: Ir.Mux => entries("Mux")
          case _: Ir.RegRead => entries("RegRead")
        }
      }
    }
}
