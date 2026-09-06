package P4C

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets

/** X6：延迟模型外置单测（对标 XLS --delay_model）。 */
class DelayModelSpec extends AnyFlatSpec with Matchers {

  import Ir._

  /** n 个 InputRef 的左结合加法链。 */
  private def chainDag(nRefs: Int): Dag = {
    val b = new Builder
    var acc: (NodeId, Int) = (b.add(InputRef(Seq("m", "x0"), 8)), 8)
    for (i <- 1 until nRefs) {
      val r = (b.add(InputRef(Seq("m", s"x$i"), 8)), 8)
      acc = b.bin(Add, acc, r)
    }
    b.finish(Seq(OutputWrite(Seq("m", "acc"), acc._1, 8)))
  }

  /** Cat/Slice 布线 + Bin 混合 DAG（Cat/Slice 权重在两模型下不同）。 */
  private def mixedDag(): (Dag, NodeId, NodeId) = {
    val b = new Builder
    val xs = (0 until 2).map(i => (b.add(InputRef(Seq("m", s"x$i"), 8)), 8))
    val cat = (b.add(Cat(xs.map(_._1), 16)), 16)
    val sl = (b.add(Slice(cat._1, 15, 0)), 16)
    val add = b.bin(Add, xs(0), xs(1))
    val dag = b.finish(Seq(
      OutputWrite(Seq("m", "w"), sl._1, 16),
      OutputWrite(Seq("m", "v"), add._1, 8),
    ))
    (dag, cat._1, sl._1)
  }

  behavior.of("内置延迟模型")

  it should "weighted（默认）：Cat/Slice 权重 0 → 第 0 级（历史行为不变）" in {
    val (dag, cat, sl) = mixedDag()
    val s = Scheduler.schedule(dag, 4, model = DelayModels.Weighted)
    s.stages(cat) should be(0)
    s.stages(sl) should be(0)
  }

  it should "unit：Cat/Slice 权重 1 → 占独立级（对照）" in {
    val (dag, cat, sl) = mixedDag()
    val s = Scheduler.schedule(dag, 4, model = DelayModels.Unit)
    // unit 深度：cat=1, sl=2, add=1 → W=2 → n=min(4,3)=3
    s.stageCount should be(3)
    s.stages(cat) should be(1)
    s.stages(sl) should be(2)
  }

  it should "RegRead 权重：weighted=2 vs unit=1" in {
    def regReadDag(): (Dag, NodeId) = {
      val b = new Builder
      val idx = (b.add(InputRef(Seq("m", "idx"), 8)), 8)
      val (rr, w) = b.regRead("stats", idx._1, idx._2, 16, 8)
      (b.finish(Seq(RegWrite("stats", idx._1, rr, w, 8))), rr)
    }
    val (dagW, rrW) = regReadDag()
    val (dagU, rrU) = regReadDag()
    // weighted：rr arrival=2 → clock=1 不可行（minClock=2）
    Scheduler.minClock(dagW, DelayModels.Weighted) should be(2)
    val e = intercept[P4Error] { Scheduler.minFeasibleStages(dagW, 1, model = DelayModels.Weighted) }
    e.getMessage should include("最小可行 clock = 2")
    // unit：rr arrival=1 → clock=1 可行（n=2）
    Scheduler.minClock(dagU, DelayModels.Unit) should be(1)
    Scheduler.minFeasibleStages(dagU, 1, model = DelayModels.Unit) should be(2)
  }

  behavior.of("外部 JSON 延迟模型")

  private def writeModel(dir: java.nio.file.Path, json: String): String = {
    val p = dir.resolve("model.json")
    java.nio.file.Files.write(p, json.getBytes(StandardCharsets.UTF_8))
    p.toString
  }

  it should "自定义权重参与调度（Bin=2 加重加法链）" in {
    val dir = java.nio.file.Files.createTempDirectory("p4c-dm")
    val path = writeModel(dir, """{"Const":0,"InputRef":0,"Cat":0,"Slice":0,"Zext":0,"Trunc":0,"Not":0,"Bin":2,"Mux":1,"RegRead":2}""")
    val model = DelayModels.load(path)
    model.name should be(path)
    // 16 个 Bin：weighted W=16 → clock=2 时 8 级；Bin=2 → W=32 → clock=2 时 16 级
    val dag = chainDag(17)
    Scheduler.minFeasibleStages(dag, 2, model = DelayModels.Weighted) should be(8)
    Scheduler.minFeasibleStages(dag, 2, model = model) should be(16)
  }

  it should "Bin 按运算符细分：Bin(Add)=3 覆盖 Bin=1" in {
    val dir = java.nio.file.Files.createTempDirectory("p4c-dm")
    val path = writeModel(dir,
      """{"Const":0,"InputRef":0,"Cat":0,"Slice":0,"Zext":0,"Trunc":0,"Not":0,"Bin":1,"Bin(Add)":3,"Mux":1,"RegRead":2}""")
    val model = DelayModels.load(path)
    Scheduler.minClock(chainDag(2), model) should be(3) // 链中唯一的运算是 Add
  }

  it should "缺少必需权重项或文件不存在 → P4Error" in {
    val dir = java.nio.file.Files.createTempDirectory("p4c-dm")
    val bad = writeModel(dir, """{"Const":0,"Bin":1}""")
    val e = intercept[P4Error] { DelayModels.load(bad) }
    e.getMessage should include("缺少权重项")
    e.getMessage should include("Mux")
    intercept[P4Error] { DelayModels.load("/nonexistent/model.json") }
      .getMessage should include("无法读取延迟模型文件")
  }

  it should "JSON 允许小数权重（ND2 倍数口径）" in {
    val dir = java.nio.file.Files.createTempDirectory("p4c-dm")
    val path = writeModel(dir,
      """{"Const":0,"InputRef":0,"Cat":0,"Slice":0,"Zext":0,"Trunc":0,"Not":0.6,"Bin":1.6,"Mux":1.2,"RegRead":3.6}""")
    val model = DelayModels.load(path)
    model.weight(Ir.Not(0, 8)) should be(0.6 +- 1e-9)
    model.weight(Ir.Mux(0, 1, 2, 8)) should be(1.2 +- 1e-9)
  }

  // ---------------- X7：Logic Effort 模型（ND2 归一化） ----------------

  behavior.of("LogicalEffort 模型（相对 ND2 的延迟倍数）")

  import Ir._

  it should "单门 op 的 ND2 倍数符合 LE 理论值" in {
    val m = DelayModels.LogicalEffort
    m.weight(Not(0, 8)) should be(0.6 +- 1e-9) // INV：2τ / 3.33τ
    m.weight(Mux(0, 1, 2, 8)) should be(1.2 +- 1e-9) // 2:1 mux：4τ / 3.33τ
    m.weight(Bin(And, 0, 1, 8)) should be(1.6 +- 1e-9) // NAND + INV
    m.weight(Bin(Or, 0, 1, 8)) should be(1.6 +- 1e-9) // NOR + INV
    m.weight(Bin(Xor, 0, 1, 8)) should be(3.0 +- 1e-9) // XOR2：10τ / 3.33τ
    // 布线节点为 0（与 LE 无关）
    m.weight(Const(0, 8)) shouldBe 0.0
    m.weight(Slice(0, 7, 0)) shouldBe 0.0
    m.weight(Cat(Seq(0), 8)) shouldBe 0.0
  }

  it should "复合 op 随位宽展开：加法器 w 级、桶形移位 log2(w) 级 mux、比较器树" in {
    val m = DelayModels.LogicalEffort
    m.weight(Bin(Add, 0, 1, 16)) should be(16.0 +- 1e-9) // 行波进位上界
    m.weight(Bin(Add, 0, 1, 8)) should be(8.0 +- 1e-9) // 与位宽线性
    m.weight(Bin(Shl, 0, 1, 16)) should be(1.2 * 4 +- 1e-9) // 4 级 2:1 mux
    m.weight(Bin(Eq, 0, 1, 16)) should be(3.0 + 1.6 * 4 +- 1e-9) // XNOR + AND 树
    m.weight(Bin(Lt, 0, 1, 16)) should be(3.0 + 2.4 * 4 +- 1e-9)
    // 8 项寄存器堆读：3 级 2:1 mux
    m.weight(RegRead("stats", 0, 8, 8)) should be(1.2 * 3 +- 1e-9)
  }

  it should "clock 预算 = 每拍可容纳的 ND2 级数：加法链按 LE 与 weighted 给出不同划分" in {
    // 16-bit 加法链 3 项（W_LE = 48）：clock=24 → LE 需 2 级（每级 1 个 Add=16 ≤ 24 不够，2 个 Add=32 > 24）
    val b = new Builder
    val xs = (0 until 4).map(i => (b.add(InputRef(Seq("m", s"x$i"), 16)), 16))
    val a1 = b.bin(Add, xs(0), xs(1))
    val a2 = b.bin(Add, a1, xs(2))
    val a3 = b.bin(Add, a2, xs(3))
    val dag = b.finish(Seq(OutputWrite(Seq("m", "acc"), a3._1, 16)))
    // weighted：每个 Add=1，W=3，clock=2 → 2 级（每级 2 个 Add）
    Scheduler.minFeasibleStages(dag, 2, model = DelayModels.Weighted) should be(2)
    // LE：Add=16，W=48，clock=24 → 每级关键延迟 = 后段 2 个 Add 相继 = 16+16 = 32 > 24 → 3 级
    //（n=2 时末级 arrival 32 与 48 相邻 → delay 32 > 24；n=3 时每级恰 1 个 Add = 16 ≤ 24）
    Scheduler.minFeasibleStages(dag, 24, model = DelayModels.LogicalEffort) should be(3)
    val sLe = Scheduler.schedule(dag, 3, model = DelayModels.LogicalEffort)
    Scheduler.stageDelays(sLe, DelayModels.LogicalEffort).foreach(_ should be <= 24.0)
    // clock=8：每级至多 0.5 个 Add → 3 级（每级恰好 1 个 Add=16 > 8 → 不可行下界 16）
    val e = intercept[P4Error] { Scheduler.minFeasibleStages(dag, 8, model = DelayModels.LogicalEffort) }
    e.getMessage should include("最小可行 clock = 16")
  }

  it should "LE 语义等价：LE 调度结果与 N=1 组合行为一致（Interp 交叉验证）" in {
    val b = new Builder
    val x = (b.add(InputRef(Seq("m", "x"), 16)), 16)
    val y = (b.add(InputRef(Seq("m", "y"), 16)), 16)
    val add = b.bin(Add, x, y)
    val eq = b.bin(Eq, add, (b.add(Const(7, 16)), 16))
    val dag = b.finish(Seq(
      OutputWrite(Seq("f0"), add._1, 16),
      OutputWrite(Seq("f1"), eq._1, 1)))
    val opt = Passes.runAll(dag)
    val n = Scheduler.minFeasibleStages(opt, 20, model = DelayModels.LogicalEffort)
    val sched = if (n > 1) Scheduler.schedule(opt, n, model = DelayModels.LogicalEffort) else opt
    val rng = new scala.util.Random(7)
    for (_ <- 0 until 30) {
      val env = Interp.Env(inputs = Map(Seq("m", "x") -> BigInt(16, rng), Seq("m", "y") -> BigInt(16, rng)))
      Interp.eval(opt, env).outputs shouldBe Interp.eval(sched, env).outputs
    }
  }
}
