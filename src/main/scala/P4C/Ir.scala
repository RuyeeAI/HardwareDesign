package P4C

/** 核心 IR：ActionDAG —— 位向量无循环数据流图（XLS 式 node-based IR）。
  *
  * 不变量：
  *   - 每个节点的 width 即其 Chisel UInt 的精确宽度；
  *   - Bin 运算的两个操作数经 [[fit]] 归一为相同宽度（Zext/Trunc 显式节点）；
  *   - 一切位宽不匹配都体现为显式 Cast 节点，绝不静默截断（ParserCore 教训）。
  */
object Ir {

  type NodeId = Int

  sealed trait Op
  case object Add extends Op
  case object Sub extends Op
  case object And extends Op
  case object Or extends Op
  case object Xor extends Op
  case object Shl extends Op
  case object Shr extends Op
  case object Eq extends Op
  case object Neq extends Op
  case object Lt extends Op
  case object Le extends Op
  case object Gt extends Op
  case object Ge extends Op

  sealed trait Node { def width: Int }

  final case class Const(value: BigInt, width: Int) extends Node
  /** 零扩展 */
  final case class Zext(src: NodeId, width: Int) extends Node
  /** 高位截断 */
  final case class Trunc(src: NodeId, width: Int) extends Node
  final case class Bin(op: Op, l: NodeId, r: NodeId, width: Int) extends Node
  /** 位切片 [hi:lo]，width = hi-lo+1 */
  final case class Slice(src: NodeId, hi: Int, lo: Int) extends Node {
    override def width: Int = hi - lo + 1
  }
  /** 拼接，parts[0] 为最高位 */
  final case class Cat(parts: Seq[NodeId], width: Int) extends Node
  final case class Mux(c: NodeId, t: NodeId, f: NodeId, width: Int) extends Node
  final case class Not(src: NodeId, width: Int) extends Node

  /** 状态单元读：reg[old][index]，width = 元素宽度 */
  final case class RegRead(inst: String, index: NodeId, width: Int, size: Int) extends Node

  /** 读取输入（header/metadata 字段或 action 参数），path 为点分路径 */
  final case class InputRef(path: Seq[String], width: Int) extends Node

  /** DAG 汇点：一次赋值 / 一次状态单元写 */
  sealed trait Sink
  final case class OutputWrite(path: Seq[String], value: NodeId, width: Int) extends Sink
  /** 寄存器写（fire 时生效） */
  final case class RegWrite(inst: String, index: NodeId, value: NodeId, width: Int, size: Int) extends Sink
  /** 计数器累加（fire 时生效，delta 通常为 1） */
  final case class CounterAdd(inst: String, index: NodeId, delta: NodeId, width: Int, size: Int) extends Sink

  /** 一次 ActionDAG 构建结果（一个 action / 一个表项 / 一段 apply 直行代码）。
    *
    * @param stages 切拍调度标注：NodeId → 所在级（0 基）。空 map = 未调度（全组合单拍）。
    *               只能由 [[Scheduler]]（在 [[Passes.runAll]] 之后）产出——优化 pass 会
    *               重编号 NodeId，且 CSE 不得在调度后运行（会跨级合并）。
    */
  final case class Dag(nodes: Vector[Node], outputs: Seq[Sink], stages: Map[NodeId, Int] = Map.empty) {
    /** stages 为空 = 未调度（全组合单拍） */
    def isScheduled: Boolean = stages.nonEmpty
    /** 实际级数：未调度 = 1；已调度 = max(stage) + 1（所有 Sink 固定末级，为发射约定，不入 map） */
    def stageCount: Int = if (stages.isEmpty) 1 else stages.values.max + 1
  }

  /** 节点操作数（按声明序；叶子节点返回空）。 */
  def operands(n: Node): Seq[NodeId] = n match {
    case z: Zext => Seq(z.src)
    case t: Trunc => Seq(t.src)
    case nt: Not => Seq(nt.src)
    case s: Slice => Seq(s.src)
    case c: Cat => c.parts
    case m: Mux => Seq(m.c, m.t, m.f)
    case b: Bin => Seq(b.l, b.r)
    case rr: RegRead => Seq(rr.index)
    case _: Const | _: InputRef => Seq.empty
  }

  /** Sink 引用的节点遍历（OutputWrite 只有 value；Reg/Counter 写含 index 与 value/delta）。 */
  def visitSink(s: Sink, v: NodeId => Unit): Unit = s match {
    case o: OutputWrite => v(o.value)
    case r: RegWrite => v(r.index); v(r.value)
    case c: CounterAdd => v(c.index); v(c.delta)
  }

  /** 便捷 DAG 构建器 */
  final class Builder {
    private val nodes = scala.collection.mutable.ArrayBuffer.empty[Node]
    def add(n: Node): NodeId = { nodes += n; nodes.length - 1 }
    def apply(id: NodeId): Node = nodes(id)
    def size: Int = nodes.length

    private def log2Ceil(n: Int): Int = math.max(1, BigInt(n - 1).bitLength)

    def fit(id: NodeId, from: Int, to: Int): NodeId =
      if (from == to) id
      else if (from < to) add(Zext(id, to))
      else add(Trunc(id, to))

    def regRead(inst: String, index: NodeId, indexWidth: Int, width: Int, size: Int): (NodeId, Int) = {
      val idxW = math.max(1, log2Ceil(size))
      val idx = fit(index, indexWidth, idxW)
      (add(RegRead(inst, idx, width, size)), width)
    }

    def regWrite(inst: String, index: NodeId, indexWidth: Int, value: NodeId, vWidth: Int, width: Int, size: Int): Sink = {
      val idxW = math.max(1, log2Ceil(size))
      RegWrite(inst, fit(index, indexWidth, idxW), fit(value, vWidth, width), width, size)
    }

    def counterAdd(inst: String, index: NodeId, indexWidth: Int, delta: NodeId, dWidth: Int, width: Int, size: Int): Sink = {
      val idxW = math.max(1, log2Ceil(size))
      CounterAdd(inst, fit(index, indexWidth, idxW), fit(delta, dWidth, width), width, size)
    }

    def bin(op: Op, l: (NodeId, Int), r: (NodeId, Int)): (NodeId, Int) = {
      val w = math.max(l._2, r._2)
      val li = fit(l._1, l._2, w)
      val ri = fit(r._1, r._2, w)
      (add(Bin(op, li, ri, w)), w)
    }

    def finish(outputs: Seq[Sink]): Dag = Dag(nodes.toVector, outputs)
  }
}

/** IR 优化 pass。输入输出均为不可变 [[Ir.Dag]]。 */
object Passes {

  import Ir._

  private def log2Ceil(n: Int): Int = math.max(1, BigInt(n - 1).bitLength)

  private def mapSink(s: Sink, f: NodeId => NodeId): Sink = s match {
    case o: OutputWrite => OutputWrite(o.path, f(o.value), o.width)
    case r: RegWrite => RegWrite(r.inst, f(r.index), f(r.value), r.width, r.size)
    case c: CounterAdd => CounterAdd(c.inst, f(c.index), f(c.delta), c.width, c.size)
  }

  private def visitSink(s: Sink, v: NodeId => Unit): Unit = s match {
    case o: OutputWrite => v(o.value)
    case r: RegWrite => v(r.index); v(r.value)
    case c: CounterAdd => v(c.index); v(c.delta)
  }

  /** 常量折叠：Const 参与的纯运算直接算出。 */
  def constFold(dag: Dag): Dag = {
    val b = new Builder
    val map = scala.collection.mutable.HashMap.empty[NodeId, NodeId]

    def value(id: NodeId): (BigInt, Int) = dag.nodes(id) match {
      case Const(v, w) => (v, w)
      case other => throw new P4Error(s"常量折叠内部错误：节点 $id 非常量 ($other)")
    }

    def go(id: NodeId): NodeId = map.getOrElseUpdate(id, {
      val nn = dag.nodes(id) match {
        case c: Const => c
        case z: Zext => dag.nodes(go(z.src)) match {
          case Const(v, w) if w < z.width => Const(v, z.width)
          case s => Zext(map(go(z.src)), z.width)
        }
        case t: Trunc => dag.nodes(go(t.src)) match {
          case Const(v, w) if w > t.width => Const(v & ((BigInt(1) << t.width) - 1), t.width)
          case s => Trunc(map(go(t.src)), t.width)
        }
        case n: Not => dag.nodes(go(n.src)) match {
          case Const(v, w) => Const((~v) & ((BigInt(1) << w) - 1), w)
          case s => Not(map(go(n.src)), n.width)
        }
        case sl: Slice => dag.nodes(go(sl.src)) match {
          case Const(v, _) => Const((v >> sl.lo) & ((BigInt(1) << (sl.hi - sl.lo + 1)) - 1), sl.hi - sl.lo + 1)
          case s => Slice(map(go(sl.src)), sl.hi, sl.lo)
        }
        case cat: Cat =>
          val ps = cat.parts.map(go)
          if (ps.forall(p => dag.nodes(p).isInstanceOf[Const])) {
            var v = BigInt(0)
            ps.foreach { p => val (pv, pw) = value(p); v = (v << pw) | pv }
            Const(v, cat.width)
          } else Cat(ps, cat.width)
        case m: Mux =>
          val c = go(m.c); val t = go(m.t); val f = go(m.f)
          dag.nodes(c) match {
            case Const(cv, _) => dag.nodes(if ((cv & 1) == 1) t else f)
            case _ =>
              if (t == f) dag.nodes(t)
              else Mux(c, t, f, m.width)
          }
        case bin: Bin =>
          val lid = go(bin.l)
          val rid = go(bin.r)
          val w = bin.width
          (dag.nodes(lid), dag.nodes(rid)) match {
            case (Const(lv, _), Const(rv, _)) => Const(evalOp(bin.op, lv, rv, w), binOpWidth(bin.op, w))
            case (lc, rc) =>
              val lz = isZero(dag, lid)
              val rz = isZero(dag, rid)
              bin.op match {
                case And => if (lz || rz) Const(0, w) else Bin(bin.op, lid, rid, w)
                case Or => if (lz) dag.nodes(rid) else if (rz) dag.nodes(lid) else Bin(bin.op, lid, rid, w)
                case Xor => if (lz) dag.nodes(rid) else if (rz) dag.nodes(lid) else Bin(bin.op, lid, rid, w)
                case Add => if (lz) dag.nodes(rid) else if (rz) dag.nodes(lid) else Bin(bin.op, lid, rid, w)
                case _ => Bin(bin.op, lid, rid, w)
              }
          }
        case ref: InputRef => ref
        case rr: RegRead => RegRead(rr.inst, go(rr.index), rr.width, rr.size)
      }
      b.add(nn)
    })

    val outs = dag.outputs.map(s => mapSink(s, go))
    b.finish(outs)
  }

  private def isZero(dag: Dag, id: NodeId): Boolean =
    dag.nodes(id) match { case Const(v, _) => v == 0; case _ => false }

  private def evalOp(op: Op, l: BigInt, r: BigInt, w: BigInt): BigInt = {
    val mask = (BigInt(1) << w.toInt) - 1
    op match {
      case Add => (l + r) & mask
      case Sub => (l - r) & mask
      case And => l & r
      case Or => l | r
      case Xor => l ^ r
      case Shl => (l << r.toInt.min(4096)) & mask
      case Shr => (l >> r.toInt.min(4096)) & mask
      case Eq => if (l == r) 1 else 0
      case Neq => if (l != r) 1 else 0
      case Lt => if (l < r) 1 else 0
      case Le => if (l <= r) 1 else 0
      case Gt => if (l > r) 1 else 0
      case Ge => if (l >= r) 1 else 0
    }
  }

  private def binOpWidth(op: Op, w: Int): Int = op match {
    case Eq | Neq | Lt | Le | Gt | Ge => 1
    case _ => w
  }

  /** 公共子表达式消除：结构相同的纯节点合并。 */
  def cse(dag: Dag): Dag = {
    val b = new Builder
    val map = scala.collection.mutable.HashMap.empty[NodeId, NodeId]
    val seen = scala.collection.mutable.HashMap.empty[Node, NodeId]

    def go(id: NodeId): NodeId = map.getOrElseUpdate(id, {
      val n0 = dag.nodes(id) match {
        case z: Zext => Zext(go(z.src), z.width)
        case t: Trunc => Trunc(go(t.src), t.width)
        case n: Not => Not(go(n.src), n.width)
        case s: Slice => Slice(go(s.src), s.hi, s.lo)
        case c: Cat => Cat(c.parts.map(go), c.width)
        case m: Mux => Mux(go(m.c), go(m.t), go(m.f), m.width)
        case bin: Bin => Bin(bin.op, go(bin.l), go(bin.r), bin.width)
        case rr: RegRead => RegRead(rr.inst, go(rr.index), rr.width, rr.size)
        case other => other // Const / InputRef
      }
      // 比较/移位以外的纯运算可 CSE；操作数 ID 先递归替换，所以 key 稳定
      val canon = n0
      val cached = seen.get(canon)
      val out = cached.getOrElse { val nid = b.add(canon); seen(canon) = nid; nid }
      out
    })

    val outs = dag.outputs.map(s => mapSink(s, go))
    b.finish(outs)
  }

  /** 死代码消除：从 outputs 可达的节点才保留。 */
  def dce(dag: Dag): Dag = {
    val keep = scala.collection.mutable.BitSet.empty
    def visit(id: NodeId): Unit = {
      if (!keep(id)) {
        keep(id) = true
        dag.nodes(id) match {
          case z: Zext => visit(z.src)
          case t: Trunc => visit(t.src)
          case n: Not => visit(n.src)
          case s: Slice => visit(s.src)
          case c: Cat => c.parts.foreach(visit)
          case m: Mux => visit(m.c); visit(m.t); visit(m.f)
          case bin: Bin => visit(bin.l); visit(bin.r)
          case rr: RegRead => visit(rr.index)
          case _ =>
        }
      }
    }
    dag.outputs.foreach(s => visitSink(s, visit))
    val b = new Builder
    val map = scala.collection.mutable.HashMap.empty[NodeId, NodeId]
    def go(id: NodeId): NodeId = map.getOrElseUpdate(id, {
      val nn = dag.nodes(id) match {
        case z: Zext => Zext(go(z.src), z.width)
        case t: Trunc => Trunc(go(t.src), t.width)
        case n: Not => Not(go(n.src), n.width)
        case s: Slice => Slice(go(s.src), s.hi, s.lo)
        case c: Cat => Cat(c.parts.map(go), c.width)
        case m: Mux => Mux(go(m.c), go(m.t), go(m.f), m.width)
        case bin: Bin => Bin(bin.op, go(bin.l), go(bin.r), bin.width)
        case rr: RegRead => RegRead(rr.inst, go(rr.index), rr.width, rr.size)
        case other => other
      }
      b.add(nn)
    })
    val outs = dag.outputs.map(s => mapSink(s, go))
    b.finish(outs)
  }

  /** 标准优化链。 */
  def runAll(dag: Dag): Dag = dce(cse(constFold(dag)))
}
