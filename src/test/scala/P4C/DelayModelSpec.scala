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
}
