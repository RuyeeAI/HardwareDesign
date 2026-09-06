package P4C

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Scheduler 纯 Scala 单测（无 chiseltest）：深度分桶正确性、Sink 末级约定、
  * 预算降级收敛、非法预算报错、budget=1 恒等、D3 断言。 */
class SchedulerSpec extends AnyFlatSpec with Matchers {

  import Ir._

  /** n 个 InputRef 的左结合加法链：Bin 节点数 = n-1，拓扑深度 D = n-1。 */
  private def chainDag(nRefs: Int): Dag = {
    val b = new Builder
    var acc: (NodeId, Int) = (b.add(InputRef(Seq("m", "x0"), 8)), 8)
    for (i <- 1 until nRefs) {
      val r = (b.add(InputRef(Seq("m", s"x$i"), 8)), 8)
      acc = b.bin(Add, acc, r)
    }
    b.finish(Seq(OutputWrite(Seq("m", "acc"), acc._1, 8)))
  }

  /** 全图单调性：操作数所在级 ≤ 使用者所在级（D2 均匀分桶的结构保证）。 */
  private def assertMonotone(dag: Dag): Unit = {
    dag.nodes.indices.foreach { id =>
      if (dag.stages.contains(id)) {
        Ir.operands(dag.nodes(id)).foreach { op =>
          val os = dag.stages.getOrElse(op,
            fail(s"节点 $id 的操作数 $op 未被调度（不可达节点不应被引用）"))
          withClue(s"节点 $id(stage=${dag.stages(id)}) 的操作数 $op(stage=$os)") {
            os should be <= dag.stages(id)
          }
        }
      }
    }
  }

  behavior.of("Scheduler.schedule（D2 深度均匀分桶）")

  it should "链深 D=6 预算 3 → 级数 3，叶子级 0，最深节点级 2" in {
    val dag = chainDag(7) // 6 个 Bin，D=6
    val s = Scheduler.schedule(dag, 3, "test/chain6")
    s.isScheduled should be(true)
    s.stageCount should be(3)
    // 叶子（Const/InputRef）深度 0 → 级 0
    s.nodes.zipWithIndex.foreach { case (n, id) =>
      if (n.isInstanceOf[InputRef] || n.isInstanceOf[Const]) s.stages(id) should be(0)
    }
    // 最深节点（链顶）→ 级 n-1 = 2
    val top = s.outputs.head.asInstanceOf[OutputWrite].value
    s.stages(top) should be(2)
    assertMonotone(s)
  }

  it should "预算大于深度时 n 收敛为 D+1（自然降级，不报错）" in {
    val dag = chainDag(7) // D=6
    val s = Scheduler.maybeSchedule(dag, 10)
    s.stageCount should be(7) // min(10, D+1) = 7
    val top = s.outputs.head.asInstanceOf[OutputWrite].value
    s.stages(top) should be(6)
    assertMonotone(s)
  }

  it should "budget=1 时原样返回（stages 空 = 未调度）" in {
    val dag = chainDag(7)
    Scheduler.maybeSchedule(dag, 1) should be theSameInstanceAs dag
    Scheduler.maybeSchedule(dag, 1).isScheduled should be(false)
    Scheduler.maybeSchedule(dag, 1).stageCount should be(1)
  }

  it should "budget < 1 时抛 P4Error 且携带 ctx" in {
    val dag = chainDag(3)
    val e = intercept[P4Error] { Scheduler.maybeSchedule(dag, 0, "control X/action a") }
    e.getMessage should include("N 必须 ≥ 1")
    e.getMessage should include("control X/action a")
    intercept[P4Error] { Scheduler.maybeSchedule(dag, -2) }
  }

  it should "不可达节点不进入调度（只调度 outputs 可达节点）" in {
    val b = new Builder
    val x = (b.add(InputRef(Seq("m", "x"), 8)), 8)
    val y = (b.add(InputRef(Seq("m", "y"), 8)), 8)
    val used = b.bin(Add, x, y)
    val dead = b.bin(Add, x, x) // 已构建但不接到 outputs
    val dag = b.finish(Seq(OutputWrite(Seq("m", "o"), used._1, 8)))
    val s = Scheduler.schedule(dag, 4)
    s.stages.contains(dead._1) should be(false)
    s.stages.contains(used._1) should be(true)
  }

  it should "RegRead 按普通节点处理（不强制第 0 级）" in {
    val b = new Builder
    val idx = (b.add(InputRef(Seq("m", "idx"), 8)), 8)
    val (rr, w) = b.regRead("stats", idx._1, idx._2, 16, 8)
    val dag = b.finish(Seq(RegWrite("stats", idx._1, rr, w, 8)))
    val s = Scheduler.schedule(dag, 4)
    // idx 深度 0 → 级 0；RegRead 深度 1 → 级 ≥ 1
    s.stages(rr) should be >= 1
    assertMonotone(s)
  }

  it should "D3：Sink 固定末级约定下正常调度不触发断言" in {
    val b = new Builder
    val idx = (b.add(InputRef(Seq("m", "idx"), 8)), 8)
    val (rr, w) = b.regRead("stats", idx._1, idx._2, 16, 8)
    val inc = (b.add(Const(1, 16)), 16)
    val v = b.bin(Add, (rr, w), inc)
    val dag = b.finish(Seq(RegWrite("stats", idx._1, v._1, 16, 8)))
    // Sink 固定末级：RegRead(级≥1) 恒在写（末级）之前，调度应通过
    noException should be thrownBy Scheduler.schedule(dag, 4, "control C/action bump")
  }

  behavior.of("Scheduler.checkReadWrite（D3 防御性断言）")

  it should "读级 > 写级时抛 P4Error（注入人为映射验证断言可触发）" in {
    val b = new Builder
    val idx = (b.add(InputRef(Seq("m", "idx"), 8)), 8)
    val (rr, w) = b.regRead("stats", idx._1, idx._2, 16, 8)
    val v = (b.add(InputRef(Seq("m", "v"), 16)), 16)
    val dag = b.finish(Seq(RegWrite("stats", idx._1, v._1, 16, 8)))
    // 人为构造：RegRead 在第 2 级，而 RegWrite（Sink）在第 1 级 → 跨级破坏读旧值语义
    val badStages = Map[Int, Int](rr -> 2, idx._1 -> 0, v._1 -> 0)
    val e = intercept[P4Error] {
      Scheduler.checkReadWrite(dag, badStages, Seq(1), "control C/action bump")
    }
    e.getMessage should include("RegRead('stats')")
    e.getMessage should include("第 2 级")
    e.getMessage should include("第 1 级")
  }

  it should "写级 ≥ 读级时通过（含多写取最小写级）" in {
    val b = new Builder
    val idx = (b.add(InputRef(Seq("m", "idx"), 8)), 8)
    val (rr, w) = b.regRead("stats", idx._1, idx._2, 16, 8)
    val v = (b.add(InputRef(Seq("m", "v"), 16)), 16)
    val dag = b.finish(Seq(
      RegWrite("stats", idx._1, v._1, 16, 8),
      CounterAdd("hits", idx._1, b.add(Const(1, 32)), 32, 8),
    ))
    val stages = Map[Int, Int](rr -> 2, idx._1 -> 0, v._1 -> 0)
    noException should be thrownBy Scheduler.checkReadWrite(dag, stages, Seq(2, 3))
    // 同名多写取 min：一个写在级 1 < 读级 2 → 触发
    val e = intercept[P4Error] {
      Scheduler.checkReadWrite(dag, stages, Seq(1, 3), "ctx")
    }
    e.getMessage should include("stats")
  }

  // ---------------- E1：加权延时模型 ----------------

  /** 混合 DAG：零代价布线（Cat/Slice）+ Bin 加法链。
    *   加权深度：InputRef/Cat/Slice = 0；b1 = 1；b2 = 2；b3 = 3 → W = 3；
    *   无权深度：InputRef = 0；cat = 1；sl = 2；b1 = 1；b2 = 2；b3 = 3 → D = 3。 */
  private def mixedDag(): (Dag, NodeId, NodeId, NodeId, NodeId) = {
    val b = new Builder
    val xs = (0 until 4).map(i => (b.add(InputRef(Seq("m", s"x$i"), 8)), 8))
    val cat = (b.add(Cat(xs.map(_._1), 32)), 32)
    val sl = (b.add(Slice(cat._1, 31, 0)), 32)
    val b1 = b.bin(Add, xs(0), xs(1))
    val b2 = b.bin(Add, b1, xs(2))
    val b3 = b.bin(Add, b2, xs(3))
    val dag = b.finish(Seq(
      OutputWrite(Seq("m", "w"), sl._1, 32),
      OutputWrite(Seq("m", "v"), b3._1, 8),
    ))
    (dag, cat._1, sl._1, b1._1, b3._1)
  }

  behavior.of("Scheduler.schedule（E1 加权分桶）")

  it should "加权把零代价节点（Cat/Slice/InputRef）聚到第 0 级，Bin 链按代价均布" in {
    val (dag, cat, sl, b1, b3) = mixedDag()
    val s = Scheduler.schedule(dag, 4, "test/mixed") // W=3 → n=min(4,4)=4
    s.isScheduled should be(true)
    s.stageCount should be(4)
    s.stages(cat) should be(0) // Cat 权重 0 → 第 0 级（旧深度模型下会占独立级）
    s.stages(sl) should be(0)  // Slice 权重 0
    s.stages(b1) should be(1)
    s.stages(b3) should be(3)  // 最深 Bin → 末级
    assertMonotone(s)
  }

  it should "无权对照（weighted=false）保持旧深度分桶：Cat/Slice 占独立级" in {
    val (dag, cat, sl, b1, b3) = mixedDag()
    val s = Scheduler.schedule(dag, 4, "test/mixed-unw", weighted = false) // D=3 → n=4
    s.isScheduled should be(true)
    s.stageCount should be(4)
    s.stages(cat) should be(1) // 深度 1 → 级 1（对照：加权下为 0）
    s.stages(sl) should be(2)  // 深度 2 → 级 2（对照：加权下为 0）
    s.stages(b3) should be(3)
    assertMonotone(s)
  }

  it should "W=0（全布线 DAG，仅 Cat/Slice/InputRef）不除零：等同 budget=1 不调度" in {
    val b = new Builder
    val x0 = b.add(InputRef(Seq("m", "a"), 8))
    val x1 = b.add(InputRef(Seq("m", "b"), 8))
    val cat = b.add(Cat(Seq(x0, x1), 16))
    val sl = b.add(Slice(cat, 7, 0))
    val dag = b.finish(Seq(OutputWrite(Seq("m", "o"), sl, 8)))
    // 旧公式下 D=2 会切成 2 级伪流水；加权下 W=0 → 原样返回（无切拍产物）
    val s = Scheduler.schedule(dag, 4, "test/wiring")
    s should be theSameInstanceAs dag
    s.isScheduled should be(false)
    s.stageCount should be(1)
    noException should be thrownBy Scheduler.maybeSchedule(dag, 8)
  }

  it should "RegRead 权重 2 影响分桶：Zext 归零级、RegRead 深于无权对照之外的布线" in {
    val b = new Builder
    val idx = (b.add(InputRef(Seq("m", "idx"), 8)), 8)
    val (rr, w) = b.regRead("stats", idx._1, idx._2, 16, 8) // fit 产生 Trunc（权重 0）
    val fitN: NodeId = b(rr) match {
      case RegRead(_, idx, _, _) => idx
      case other => sys.error(s"期望 RegRead，got $other")
    }
    val dag = b.finish(Seq(RegWrite("stats", fitN, rr, 16, 8)))
    val sw = Scheduler.schedule(dag, 4, "test/rr-w") // W=2 → n=min(4,3)=3；rr wd=2 → 级 2
    sw.stages(rr) should be(2)
    sw.stages(fitN) should be(0) // Trunc 权重 0 → 第 0 级
    val su = Scheduler.schedule(dag, 4, "test/rr-unw", weighted = false) // D=2 → n=3；Trunc 深度 1
    su.stages(fitN) should be(1) // 对照：无权下 Trunc 占第 1 级
    su.stages(rr) should be(2)
    assertMonotone(sw)
    assertMonotone(su)
  }

  // ---------------- X2：时钟约束模式 ----------------

  behavior.of("Scheduler.minClock / stageDelays / minFeasibleStages（clock 模式）")

  it should "minClock = 最大单节点权重（Bin 链 = 1）" in {
    Scheduler.minClock(chainDag(7)) should be(1)
    val b = new Builder
    val idx = (b.add(InputRef(Seq("m", "idx"), 8)), 8)
    val (rr, w) = b.regRead("stats", idx._1, idx._2, 16, 8)
    val dag = b.finish(Seq(RegWrite("stats", idx._1, rr, w, 8)))
    Scheduler.minClock(dag) should be(2) // RegRead 权重 2
  }

  it should "16 项加法链 clock=1 → 最小可行级数 16（每级恰一个 Bin）" in {
    val dag = chainDag(17) // 16 个 Bin，W=16
    Scheduler.minFeasibleStages(dag, 1, "test/deep") should be(16)
    // 放宽 clock：两级各 8 个 Bin → delay = 8
    Scheduler.minFeasibleStages(dag, 8) should be(2)
    Scheduler.minFeasibleStages(dag, 16) should be(1) // 单级整体 delay=W 可行
  }

  it should "W=0（全布线 DAG）clock 模式返回 1" in {
    val b = new Builder
    val x0 = b.add(InputRef(Seq("m", "a"), 8))
    val x1 = b.add(InputRef(Seq("m", "b"), 8))
    val cat = b.add(Cat(Seq(x0, x1), 16))
    val dag = b.finish(Seq(OutputWrite(Seq("m", "o"), cat, 16)))
    Scheduler.minFeasibleStages(dag, 1) should be(1)
  }

  it should "clock 低于单节点最大权重 → P4Error 并报告最小可行周期" in {
    val b = new Builder
    val idx = (b.add(InputRef(Seq("m", "idx"), 8)), 8)
    val (rr, w) = b.regRead("stats", idx._1, idx._2, 16, 8)
    val dag = b.finish(Seq(RegWrite("stats", idx._1, rr, w, 8)))
    val e = intercept[P4Error] { Scheduler.minFeasibleStages(dag, 1, "control C/action bump") }
    e.getMessage should include("最小可行 clock = 2")
    e.getMessage should include("control C/action bump")
    intercept[P4Error] { Scheduler.minFeasibleStages(dag, 0) } // clock < 1
  }

  it should "stageDelays：调度结果每级延迟 ≤ clock（minFeasibleStages 的自洽性）" in {
    val dag = chainDag(17)
    val n = Scheduler.minFeasibleStages(dag, 2)
    val s = Scheduler.schedule(dag, n, "test/selfcheck")
    Scheduler.stageDelays(s).foreach(_ should be <= 2.0)
    // 未调度 DAG = 单级，delay = max arrival
    Scheduler.stageDelays(dag) should be(Seq(16))
  }
}
