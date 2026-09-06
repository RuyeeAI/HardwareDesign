package P4C

/** 延迟模型（对标 XLS `--delay_model`）：为调度器提供节点的整数逻辑代价（权重）。
  *
  * 权重口径与 E1 一致：0 = 纯布线/零逻辑；k = k 级逻辑深度当量。调度器据此计算
  * 加权深度 arrival(x) 并分桶；X2 的 clock 模式（每级组合延迟上限）同样以该口径
  * 度量。未来工艺特征化模型（对标 XLS sky130/ASAP7 textproto）实现本 trait 即可接入。
  */
trait DelayModel {
  /** 模型名（日志/签名用）。 */
  def name: String
  /** 节点逻辑代价。 */
  def weight(n: Ir.Node): Int
}

object DelayModels {

  /** E1 加权表（默认，与历史行为逐字节一致）。 */
  object Weighted extends DelayModel {
    val name: String = "weighted"
    def weight(n: Ir.Node): Int = n match {
      case _: Ir.Const | _: Ir.InputRef | _: Ir.Cat | _: Ir.Slice |
           _: Ir.Zext | _: Ir.Trunc | _: Ir.Not => 0
      case _: Ir.Bin | _: Ir.Mux => 1
      case _: Ir.RegRead => 2
    }
  }

  /** XLS unit 模型对标：叶子（Const/InputRef）为 0，其余每节点 1。 */
  object Unit extends DelayModel {
    val name: String = "unit"
    def weight(n: Ir.Node): Int = n match {
      case _: Ir.Const | _: Ir.InputRef => 0
      case _ => 1
    }
  }

  val builtin: Map[String, DelayModel] = Map(Weighted.name -> Weighted, Unit.name -> Unit)

  val default: DelayModel = Weighted

  private val requiredOps = Seq("Const", "InputRef", "Cat", "Slice", "Zext", "Trunc", "Not", "Bin", "Mux", "RegRead")

  /** 解析模型规格：内置名（weighted/unit，大小写不敏感）或 JSON 文件路径。
    *
    * JSON 形如 `{"Const":0,"InputRef":0,...,"Bin":1,"Mux":1,"RegRead":2}`；
    * 可用 `"Bin(Add)": 2` 按运算符细分（未细分的运算符回落到 `"Bin"`）。
    * 缺少任一必需项 → [[P4Error]]（带模型路径与缺失项清单）。
    */
  def load(spec: String): DelayModel =
    builtin.get(spec.toLowerCase).getOrElse {
      val path = java.nio.file.Paths.get(spec)
      val txt = try new String(java.nio.file.Files.readAllBytes(path), java.nio.charset.StandardCharsets.UTF_8)
      catch { case e: java.io.IOException => throw new P4Error(s"无法读取延迟模型文件 '$spec'：${e.getMessage}") }
      val entries = """"(\w+(?:\(\w+\))?)"\s*:\s*(\d+)""".r
        .findAllMatchIn(txt).map(m => m.group(1) -> m.group(2).toInt).toMap
      val missing = requiredOps.filterNot(entries.contains)
      if (missing.nonEmpty)
        throw new P4Error(s"延迟模型文件 '$spec' 缺少权重项：${missing.mkString(", ")}")
      new DelayModel {
        val name: String = spec
        def weight(n: Ir.Node): Int = n match {
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
