package P4C

import scala.collection.mutable

/** 切拍调度 pass（D2 分桶 + E1 加权延时模型）。
  *
  * 产出调度标注（[[Ir.Dag.stages]]），不修改 DAG 本体。
  *
  * 调用时序约定（关键）：
  *   - 必须在 [[Passes.runAll]]（constFold → cse → dce）之后调用：优化 pass 会
  *     重建并重编号 NodeId，先调度后优化会作废 stages 映射；
  *   - 调度之后不得再跑 CSE 等结构等价 pass：CSE 以节点结构去重，会把"结构相同但
  *     落在不同级"的节点跨级合并，产生级间数据依赖错误。
  *
  * E1 加权延时模型（替代均匀深度分桶，默认启用）：
  *   - 节点代价 weight：Cat / Slice / Zext / Trunc / Not / Const / InputRef = 0
  *     （纯布线/零逻辑）；Bin（全部算术/比较/移位/逻辑）= 1；Mux = 1；
  *     RegRead = 2（存储读延迟）；
  *   - 加权深度 wd(n) = weight(n) + max(wd(操作数))（无操作数 = weight）；
  *   - W = 所有可达节点的 wd 最大值；**W = 0（全布线 DAG，如只有 Cat/Slice）时
  *     不存在任何需要切开的逻辑级，等同 budget=1 直接不调度**（避免除零，也避免
  *     生成只有一级的无意义流水）；
  *   - n = min(budget, W+1)（加权深度不足预算时自然降级，不报错）；
  *   - stage(x) = min(n-1, wd(x) * n / (W+1))：把加权深度区间 [0, W] 均匀映射到
  *     [0, n-1]（整数除法）。映射单调 ⇒ 操作数所在级恒 ≤ 使用者所在级；
  *   - 所有 Sink（OutputWrite/RegWrite/CounterAdd）固定末级 n-1（发射约定，不进
  *     stages map，见 [[ChiselBackend.StagedEmitter]]）。
  *
  * `weighted = false` 保留旧的均匀深度分桶（Const/InputRef 深度 0，其余
  * depth = 1 + max(操作数深度)），仅作测试对照，不用于生产路径。
  */
object Scheduler {

  /** budget == 1 时原样返回（stages 空 = 未调度 = 全组合单拍）；否则调度。
    * ctx 用于错误信息定位（如 "control Ingress/action bump"）。 */
  def maybeSchedule(dag: Ir.Dag, budget: Int, ctx: String = "", weighted: Boolean = true): Ir.Dag =
    if (budget == 1) dag else schedule(dag, budget, ctx, weighted)

  /** 节点逻辑代价（E1 权重表）：0 = 纯布线/零逻辑，1 = 一级算术/选择，2 = 存储读。 */
  private def weight(n: Ir.Node): Int = n match {
    case _: Ir.Const | _: Ir.InputRef | _: Ir.Cat | _: Ir.Slice |
         _: Ir.Zext | _: Ir.Trunc | _: Ir.Not => 0
    case _: Ir.Bin | _: Ir.Mux => 1
    case _: Ir.RegRead => 2
  }

  /** 按加权深度（默认）或均匀深度（weighted=false，测试对照）分桶调度，
    * 返回带 stages 标注的 DAG（节点集合不变）。 */
  def schedule(dag: Ir.Dag, budget: Int, ctx: String = "", weighted: Boolean = true): Ir.Dag = {
    val where = if (ctx.isEmpty) "" else s"$ctx："
    if (budget < 1) throw new P4Error(s"${where}拍数预算 N 必须 ≥ 1（got $budget）")
    if (budget == 1) return dag

    // 1. 可达性：从 outputs 出发 DFS，只调度可达节点（与 dce 的遍历模式一致）
    val reach = mutable.BitSet.empty
    def visit(id: Ir.NodeId): Unit =
      if (!reach(id)) {
        reach(id) = true
        Ir.operands(dag.nodes(id)).foreach(visit)
      }
    dag.outputs.foreach(Ir.visitSink(_, visit))
    if (reach.isEmpty) return dag // 空 DAG（如空 action）：无可调度节点

    // 2. 深度（memoized）。NodeId 升序即拓扑序（Builder 追加构造，操作数恒为更早节点）。
    //    weighted：wd = weight + max(wd(操作数))；unweighted：旧公式（Const/InputRef 0，其余 1+max）。
    val depth = mutable.HashMap.empty[Ir.NodeId, Int]
    (0 until dag.nodes.length).foreach { id =>
      if (reach(id)) {
        val base =
          if (weighted) weight(dag.nodes(id))
          else dag.nodes(id) match {
            case _: Ir.Const | _: Ir.InputRef => 0
            case _ => 1
          }
        depth(id) = base + (Ir.operands(dag.nodes(id)) match {
          case Seq() => 0
          case ops => ops.map(depth).max
        })
      }
    }
    val maxDepth = depth.values.max

    // E1：W=0 —— 全布线 DAG（可达节点全部零代价），没有可切的逻辑级：
    // 等同 budget=1，直接不调度（stages 保持空，走原组合发射路径；同时避免
    // 下面的分母 W+1 无意义地产生全 0 级"伪流水"）。
    if (weighted && maxDepth == 0) return dag

    // 3. 均匀分桶：stage(id) = min(n-1, depth(id) * n / (W+1))
    val nStages = math.min(budget, maxDepth + 1)
    val denom = maxDepth + 1
    val stageMap: Map[Ir.NodeId, Int] = depth.toMap.map { case (id, d) =>
      id -> math.min(nStages - 1, d * nStages / denom)
    }

    // 4. D3 读-写校验（防御性断言：Sink 固定末级下结构上恒过，守护未来优化）
    checkReadWrite(dag, stageMap, dag.outputs.map(_ => nStages - 1), ctx)

    dag.copy(stages = stageMap)
  }

  // ---------------- X2：时钟约束模式（对标 XLS 两阶段调度 / minimize_clock_on_failure） ----------------

  /** 可达节点的加权深度表 arrival(x)（与 [[schedule]] 加权路径同口径）。 */
  private def depths(dag: Ir.Dag): Map[Ir.NodeId, Int] = {
    val reach = mutable.BitSet.empty
    def visit(id: Ir.NodeId): Unit =
      if (!reach(id)) {
        reach(id) = true
        Ir.operands(dag.nodes(id)).foreach(visit)
      }
    dag.outputs.foreach(Ir.visitSink(_, visit))
    val depth = mutable.HashMap.empty[Ir.NodeId, Int]
    (0 until dag.nodes.length).foreach { id =>
      if (reach(id)) {
        depth(id) = weight(dag.nodes(id)) + (Ir.operands(dag.nodes(id)) match {
          case Seq() => 0
          case ops => ops.map(depth).max
        })
      }
    }
    depth.toMap
  }

  /** clock 约束的结构下界：最大单节点权重（单节点不可再切分）。
    * clockW < minClock 时不存在任何可行调度（XLS minimize_clock_on_failure 的等价报告）。 */
  def minClock(dag: Ir.Dag): Int = {
    val d = depths(dag)
    d.keys.map(id => weight(dag.nodes(id))).foldLeft(0)(math.max)
  }

  /** 调度结果的各级组合延迟（加权口径）：delay(k) = max(arrival ∈ k) − start(k)，
    * start(k) = min(arrival(x) − weight(x), x ∈ k)（本级行入点的最早到达）。
    * 未调度 DAG 视为单级：delay = max(arrival)。 */
  def stageDelays(dag: Ir.Dag): Seq[Int] = {
    val d = depths(dag)
    if (d.isEmpty) return Seq(0)
    if (!dag.isScheduled) return Seq(d.values.max)
    (0 until dag.stageCount).map { k =>
      val inStage = d.collect { case (id, a) if dag.stages.getOrElse(id, 0) == k => (id, a) }
      if (inStage.isEmpty) 0
      else {
        val start = inStage.map { case (id, a) => a - weight(dag.nodes(id)) }.min
        inStage.values.max - start
      }
    }
  }

  /** clock 模式：给定每级组合延迟上限 clockW，求最小可行级数（线性扫描 1..W+1，
    * 取首个每级延迟 ≤ clockW 的 n；扫描而非二分，规避分桶映射的非严格单调性假设）。
    * clockW 低于 [[minClock]] 时抛 [[P4Error]] 并报告最小可行周期。 */
  def minFeasibleStages(dag: Ir.Dag, clockW: Int, ctx: String = ""): Int = {
    val where = if (ctx.isEmpty) "" else s"$ctx："
    if (clockW < 1) throw new P4Error(s"${where}clock 约束必须 ≥ 1（got $clockW）")
    val mc = minClock(dag)
    if (clockW < mc)
      throw new P4Error(s"${where}clock=$clockW 不可行：单节点最大权重 $mc 已超约束（最小可行 clock = $mc）")
    val d = depths(dag)
    if (d.isEmpty || d.values.max == 0) return 1 // 全布线 DAG / 空 DAG：无需切拍
    val maxW = d.values.max
    (1 to maxW + 1).find { n =>
      stageDelays(schedule(dag, n, ctx)).forall(_ <= clockW)
    }.getOrElse(maxW + 1)
  }

  /** D3：RegRead 与同名 RegWrite/CounterAdd 跨级读-写次序校验。
    *
    * 语义约定：一次 DAG 调用内 RegRead 读旧值，所有写统一在末级提交（D3 不做写旁路）。
    * 若某 extern 实例的最大读级 > 其最小写级，读会读到本次调用的新值——编译期报
    * [[P4Error]]。
    *
    * 注意："Sink 固定末级"约定下写级恒为 n-1 ≥ 任意读级，本断言结构上不可能触发；
    * 保留以守护未来"Sink 提前到最深输入级"的优化（PRD P2 方向）不静默引入语义错误。
    *
    * @param sinkStages 各 Sink 的所在级，与 dag.outputs 一一对应；
    *                   Scheduler 按末级约定传 n-1，测试可注入人为映射验证断言。
    */
  def checkReadWrite(
    dag: Ir.Dag, stages: Map[Ir.NodeId, Int], sinkStages: Seq[Int], ctx: String = "",
  ): Unit = {
    val where = if (ctx.isEmpty) "" else s"$ctx："
    val minWrite = mutable.HashMap.empty[String, Int]
    dag.outputs.zip(sinkStages).foreach {
      case (r: Ir.RegWrite, st) => minWrite(r.inst) = math.min(minWrite.getOrElse(r.inst, Int.MaxValue), st)
      case (c: Ir.CounterAdd, st) => minWrite(c.inst) = math.min(minWrite.getOrElse(c.inst, Int.MaxValue), st)
      case _ =>
    }
    dag.nodes.indices.foreach { id =>
      dag.nodes(id) match {
        case Ir.RegRead(inst, _, _, _) if stages.contains(id) =>
          minWrite.get(inst).foreach { wSt =>
            val rSt = stages(id)
            if (rSt > wSt)
              throw new P4Error(
                s"${where}RegRead('$inst') 位于第 $rSt 级，但同名 RegWrite/CounterAdd 在第 $wSt 级写——" +
                  "跨级读-写次序破坏（会读到本次调用的新值）。D3 不做写旁路：请减小拍数预算或拆分 action")
          }
        case _ =>
      }
    }
  }
}
