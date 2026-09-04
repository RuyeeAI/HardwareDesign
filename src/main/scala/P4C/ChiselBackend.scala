package P4C

import P4C.Ast._
import P4C.Ir._
import P4C.IrBuilder._

import scala.collection.mutable

/** Chisel 后端：把 IR/AST 翻译为参数化 Chisel（Scala）源码。
  *
  *   - control → 组合 Module（M1 直行 action / M2 静态融合表）；
  *   - parser  → FSM Module（M3，512-bit 网络序报文窗口）。
  */
object ChiselBackend {

  private val IND = "  "

  private def pascal(s: String): String =
    if (s.isEmpty) s else s.charAt(0).toUpper + s.substring(1)

  // ---------------- 表达式发射 ----------------

  /** 语句发射器：公共子表达式落成 val，叶子内联。 */
  private final class Emitter(
    dag: Dag,
    readPath: Seq[String] => String,
    val indent: String,
    val fireCond: Option[String] = None,
  ) {
    private val lines = mutable.ArrayBuffer.empty[String]
    private val stateful = mutable.ArrayBuffer.empty[String]
    private val memo = mutable.HashMap.empty[NodeId, String]
    private var tmp = 0

    private def fitIdx(expr: String, w: Int, size: Int): String = {
      val idxW = math.max(1, BigInt(size - 1).bitLength)
      if (w > idxW) s"($expr)(${idxW - 1}, 0)"
      else if (w < idxW) s"Cat(0.U(${idxW - w}.W), $expr)"
      else expr
    }

    private val refCount: mutable.HashMap[NodeId, Int] = {
      val m = mutable.HashMap.empty[NodeId, Int]
      def bump(id: NodeId): Unit = m(id) = m.getOrElse(id, 0) + 1
      dag.outputs.foreach {
        case o: OutputWrite => bump(o.value)
        case r: RegWrite => bump(r.index); bump(r.value)
        case c: CounterAdd => bump(c.index); bump(c.delta)
      }
      dag.nodes.indices.foreach { id =>
        dag.nodes(id) match {
          case z: Zext => bump(z.src)
          case t: Trunc => bump(t.src)
          case n: Not => bump(n.src)
          case s: Ir.Slice => bump(s.src)
          case c: Cat => c.parts.foreach(bump)
          case m2: Mux => bump(m2.c); bump(m2.t); bump(m2.f)
          case b2: Ir.Bin => bump(b2.l); bump(b2.r)
          case _ =>
        }
      }
      m
    }

    private def cmpBool(id: NodeId): Option[String] = dag.nodes(id) match {
      case Ir.Bin(op, l, r, _) =>
        val s = op match {
          case Eq => Some("===")
          case Neq => Some("=/=")
          case Lt => Some("<")
          case Le => Some("<=")
          case Gt => Some(">")
          case Ge => Some(">=")
          case _ => None
        }
        s.map(o => s"(${go(l)} $o ${go(r)})")
      case _ => None
    }

    private def condOf(id: NodeId): String =
      cmpBool(id).getOrElse(s"(${go(id)} === 1.U)")

    private def go(id: NodeId): String = {
      if (memo.contains(id)) return memo(id)
      val n = dag.nodes(id)
      val inlineStr: String = n match {
        case Const(v, w) => s"0x${v.toString(16)}.U($w.W)"
        case InputRef(p, _) => readPath(p)
        case Ir.RegRead(inst, idx, _, size) =>
          s"reg_${inst}(${fitIdx(go(idx), dag.nodes(idx).width, size)})"
        case Zext(s, w) =>
          val sw = dag.nodes(s).width
          s"Cat(0.U(${w - sw}.W), ${go(s)})"
        case Trunc(s, w) => s"(${go(s)})(${w - 1}, 0)"
        case Not(s, _) => s"(~${go(s)})"
        case Ir.Slice(s, hi, lo) => s"(${go(s)})($hi, $lo)"
        case Cat(ps, _) => s"Cat(${ps.map(go).mkString(", ")})"
        case Mux(c, t, f, _) => s"Mux(${condOf(c)}, ${go(t)}, ${go(f)})"
        case Ir.Bin(op, l, r, w) =>
          val ls = go(l); val rs = go(r)
          op match {
            case Ir.Add => s"($ls +& $rs)"
            case Ir.Sub => s"($ls -& $rs)"
            case Ir.And => s"($ls & $rs)"
            case Ir.Or => s"($ls | $rs)"
            case Ir.Xor => s"($ls ^ $rs)"
            case Ir.Shl => s"($ls << $rs)(${w - 1}, 0)"
            case Ir.Shr => s"($ls >> $rs)"
            case _ => throw new P4Error("比较节点只能作为 Mux 条件")
          }
      }
      val isLeaf = n.isInstanceOf[Const] || n.isInstanceOf[InputRef]
      if (isLeaf || refCount.getOrElse(id, 0) <= 1) inlineStr
      else {
        val name = s"t${tmp}"
        tmp += 1
        lines += s"${indent}val $name = $inlineStr"
        memo(id) = name
        name
      }
    }

    def emitSink(s: Sink): Unit = s match {
      case o: OutputWrite =>
        lines += s"$indent${writePath(o.path)} := ${go(o.value)}"
      case r: RegWrite =>
        stateful += s"reg_${r.inst}(${fitIdx(go(r.index), dag.nodes(r.index).width, r.size)}) := ${go(r.value)}"
      case c: CounterAdd =>
        val idx = fitIdx(go(c.index), dag.nodes(c.index).width, c.size)
        stateful += s"cnt_${c.inst}($idx) := (cnt_${c.inst}($idx) +& ${go(c.delta)})"
    }

    def hasStateful: Boolean = stateful.nonEmpty

    def emitExprRoot(root: NodeId): String = go(root)

    def takeLines: Seq[String] =
      if (stateful.isEmpty) lines.toSeq
      else {
        val cond = fireCond.getOrElse("true.B")
        lines.toSeq ++ Seq(s"${indent}when ($cond) {") ++
          stateful.toSeq.map(l => s"$indent$IND$l") ++ Seq(s"$indent}")
      }

    private def writePath(path: Seq[String]): String =
      s"io.${path.head}Out.${path.drop(1).mkString(".")}"
  }

  private def readOf(param: String, rest: Seq[String]): String =
    s"io.${param}In.${rest.mkString(".")}"

  // ---------------- 类型发射 ----------------

  private def emitBundles(prog: P4Program, tmap: String => String): Seq[String] = {
    val out = mutable.ArrayBuffer.empty[String]
    out += s"${IND}// ---------- 类型 Bundle ----------"
    prog.headerTypes.foreach { ht =>
      val cn = tmap(ht.name)
      out += s"final class $cn extends Bundle {"
      ht.fields.foreach(f => out += s"${IND}val ${f.name} = UInt(${f.width}.W)")
      out += s"}"
      out += ""
    }
    prog.structs.foreach { st =>
      val cn = tmap(st.name)
      out += s"final class $cn extends Bundle {"
      st.members.foreach {
        case StructMember(_, true, w, n) => out += s"${IND}val $n = UInt($w.W)"
        case StructMember(tn, false, _, n) => out += s"${IND}val $n = new ${tmap(tn)}"
      }
      out += s"}"
      out += ""
    }
    out.toSeq
  }

  // ---------------- control 发射（M1 / M2 / M4） ----------------

  private def emitControl(prefix: String, c: ControlDecl, prog: P4Program, tmap: String => String): Seq[String] = {
    val resolver = new WidthResolver(
      prog.headerTypes.map(ht => ht.name -> ht).toMap,
      prog.structs.map(st => st.name -> st).toMap,
      c.params,
    )
    val externMap = c.externs.map(e => e.name -> e).toMap
    val stateful = c.externs.nonEmpty
    val out = mutable.ArrayBuffer.empty[String]
    val className = pascal(prefix) + pascal(c.name)
    out += s"final class $className extends Module {"
    out += s"${IND}val io = IO(new Bundle {"
    c.params.foreach {
      case ControlParam(n, "inout", t, _) =>
        out += s"$IND$IND val ${n}In = Input(new ${tmap(t)})"
        out += s"$IND$IND val ${n}Out = Output(new ${tmap(t)})"
      case ControlParam(n, "in", t, _) => out += s"$IND$IND val ${n}In = Input(new ${tmap(t)})"
      case ControlParam(n, "out", t, _) => out += s"$IND$IND val ${n}Out = Output(new ${tmap(t)})"
      case p => throw new P4Error(s"行 ${p.line}：不支持的方向 '${p.direction}'")
    }
    if (stateful) out += s"$IND$IND val valid = Input(Bool())"
    // extern 状态观察口
    c.externs.foreach { e => out += s"$IND$IND val ex_${e.name} = Output(Vec(${e.size}, UInt(${e.width}.W)))" }
    out += s"$IND})"
    out += ""

    // 状态单元（M4）：Register / Counter
    c.externs.foreach {
      case ExternInst("Register", w, size, n, _) =>
        out += s"$IND val reg_$n = RegInit(VecInit(Seq.fill($size)(0.U($w.W))))"
        out += s"$IND io.ex_$n := reg_$n"
      case ExternInst("Counter", w, size, n, _) =>
        out += s"$IND val cnt_$n = RegInit(VecInit(Seq.fill($size)(0.U($w.W))))"
        out += s"$IND io.ex_$n := cnt_$n"
      case e => throw new P4Error(s"行 ${e.line}：未知 extern '${e.kind}'")
    }
    if (stateful) out += ""

    // 透传
    c.params.foreach { p =>
      if (p.direction == "inout") out += s"$IND io.${p.name}Out := io.${p.name}In"
    }

    val actions = c.actions.map(a => a.name -> a).toMap
    val tables = c.tables.map(t => t.name -> t).toMap

    def lowerStmts(stmts: Seq[Stmt], binds: Bindings, lowering: ExprLowering): Seq[Sink] =
      stmts.map {
        case asg: Assign => lowering.lowerAssign(asg.path, asg.expr, binds)
        case mc: MethodCall => lowering.lowerMethodCall(mc, binds)
        case s => throw new P4Error(s"行 ${s.line}：action 体中不支持的语句（M4 子集：赋值 / extern 方法调用）")
      }

    val fire = if (stateful) Some("io.valid") else None

    // 发射 apply 体
    c.applyBody.foreach {
      case ActionCall(name, args, ln) =>
        val a = actions.getOrElse(name, throw new P4Error(s"行 $ln：未知 action '$name'"))
        if (args.length != a.params.length)
          throw new P4Error(s"行 $ln：action '$name' 实参数量不匹配")
        val b = new Ir.Builder
        val lowering = new ExprLowering(resolver, b, externMap)
        val binds: Bindings = a.params.zip(args).map { case (p, e) =>
          val (id, w) = lowering.lower(e, Some(p.width), Map.empty)
          p.name -> ((b.fit(id, w, p.width), p.width))
        }.toMap
        val outs = lowerStmts(a.body, binds, lowering)
        val dag = Passes.runAll(b.finish(outs))
        out += s"$IND// action $name"
        val em = new Emitter(dag, p => readOf(p.head, p.drop(1)), s"$IND", fire)
        dag.outputs.foreach(em.emitSink)
        out ++= em.takeLines
        out += ""
      case asg: Assign =>
        val b = new Ir.Builder
        val lowering = new ExprLowering(resolver, b, externMap)
        val dag0 = b.finish(Seq(lowering.lowerAssign(asg.path, asg.expr, Map.empty)))
        val dag = Passes.runAll(dag0)
        val em = new Emitter(dag, p => readOf(p.head, p.drop(1)), s"$IND", fire)
        dag.outputs.foreach(em.emitSink)
        out ++= em.takeLines
        out += ""
      case mc: MethodCall =>
        val b = new Ir.Builder
        val lowering = new ExprLowering(resolver, b, externMap)
        val dag0 = b.finish(Seq(lowering.lowerMethodCall(mc, Map.empty)))
        val dag = Passes.runAll(dag0)
        val em = new Emitter(dag, p => readOf(p.head, p.drop(1)), s"$IND", fire)
        dag.outputs.foreach(em.emitSink)
        out ++= em.takeLines
        out += ""
      case TableApply(n, ln) =>
        val t = tables.getOrElse(n, throw new P4Error(s"行 $ln：未知 table '$n'"))
        if (t.entries.isEmpty) throw new P4Error(s"行 $ln：table '$n' 无 const entries（M2 仅支持静态融合）")
        out ++= emitStaticTable(t, c, prog, resolver, externMap, stateful, s"$IND")
        out += ""
      case v: VarDecl => throw new P4Error(s"行 ${v.line}：M1/M2 暂不支持 apply 内局部变量")
      case s => throw new P4Error(s"行 ${s.line}：control 中不支持的语句")
    }

    out += "}"
    out.toSeq
  }

  /** M2：静态融合 exact 表。 */
  private def emitStaticTable(
    t: TableDecl, c: ControlDecl, prog: P4Program, resolver: WidthResolver,
    externMap: Map[String, ExternInst], stateful: Boolean, indent: String,
  ): Seq[String] = {
    val out = mutable.ArrayBuffer.empty[String]
    out += s"$indent// table ${t.name}（静态融合，${t.entries.size} 项）"

    if (t.keys.exists(_.matchKind != "exact"))
      throw new P4Error(s"table '${t.name}'：M2 仅支持 exact 匹配")

    val keyWidths = t.keys.map { k =>
      k.expr match {
        case Name(p, _) => resolver.widthOf(p)
        case other => throw new P4Error(s"table '${t.name}'：M2 key 必须是字段路径（行 ${other.line}）")
      }
    }

    // key 表达式（读取输入）
    val keyExprs = t.keys.zip(keyWidths).map { case (k, w) =>
      val b = new Ir.Builder
      val lowering = new ExprLowering(resolver, b, externMap)
      val (id, kw) = lowering.lower(k.expr, Some(w), Map.empty)
      val dag = Passes.runAll(b.finish(Seq(OutputWrite(Seq("__key"), b.fit(id, kw, w), w))))
      val em = new Emitter(dag, p => readOf(p.head, p.drop(1)), indent)
      val expr = em.emitExprRoot(dag.outputs.head.asInstanceOf[OutputWrite].value)
      (expr, em.takeLines, w)
    }
    keyExprs.zipWithIndex.foreach { case ((_, ls, _), i) => out ++= ls }
    val keyVal = keyExprs match {
      case Seq((e, _, _)) => e
      case multi =>
        // 多 key：Cat 拼接（先声明的 key 在高位）
        out += s"$indent val key = Cat(${multi.map(_._1).mkString(", ")})"
        "key"
    }
    val totalKeyWidth = keyWidths.sum

    // 命中信号
    val nonDefault = t.entries.filterNot(_.isDefault)
    nonDefault.zipWithIndex.foreach { case (e, i) =>
      val kv = combineKeys(e.keys.map(lowerConstKey(_, resolver, t.name)), keyWidths)
      out += s"$indent val hit_$i = $keyVal === 0x${kv.toString(16)}.U($totalKeyWidth.W)"
    }

    // 各字段写出：收集所有非 default 表项的写出字段
    val defaultEntry = t.entries.find(_.isDefault)
    case class EntryDag(entry: TableEntry, dag: Ir.Dag, hits: Boolean, idx: Int)
    val entryDags = nonDefault.zipWithIndex.map { case (e, i) =>
      EntryDag(e, lowerEntry(e, resolver, c, prog, externMap), hits = true, idx = i)
    } ++ defaultEntry.map(e => EntryDag(e, lowerEntry(e, resolver, c, prog, externMap), hits = false, -1))

    // 状态单元写（M4）：每个表项在其命中（或 default + io.valid）条件下写
    entryDags.foreach { ed =>
      val stateSinks = ed.dag.outputs.filter {
        case _: OutputWrite => false
        case _ => true
      }
      if (stateSinks.nonEmpty) {
        val fire = (if (stateful) "io.valid" else "true.B") +
          (if (ed.hits) s" && hit_${ed.idx}" else "")
        val em = new Emitter(ed.dag, p => readOf(p.head, p.drop(1)), indent, Some(fire))
        stateSinks.foreach(em.emitSink)
        out ++= em.takeLines
      }
    }

    val fieldOrder = mutable.LinkedHashSet.empty[Seq[String]]
    entryDags.foreach(ed => ed.dag.outputs.foreach {
      case o: OutputWrite => fieldOrder += o.path
      case _ =>
    })

    fieldOrder.foreach { path =>
      val param = path.head
      val dir = c.params.find(_.name == param).map(_.direction).getOrElse("inout")
      val readExpr = readOf(path.head, path.drop(1))
      val fallbackEntry = entryDags.find(ed => !ed.hits && ed.dag.outputs.exists {
        case o: OutputWrite => o.path == path
        case _ => false
      })
      val writeExprs = entryDags.filter(_.hits).flatMap { ed =>
        ed.dag.outputs.collectFirst { case ow: OutputWrite if ow.path == path => ow }.map { ow =>
          val em = new Emitter(ed.dag, p => readOf(p.head, p.drop(1)), indent)
          val e = em.emitExprRoot(ow.value)
          (em.takeLines, e)
        }
      }
      writeExprs.foreach { case (ls, _) => out ++= ls }
      val muxPairs = writeExprs.zipWithIndex.map { case ((_, e), i) => s"hit_$i -> $e" }
      val rhs = fallbackEntry match {
        case Some(fed) =>
          val ow = fed.dag.outputs.collectFirst { case o: OutputWrite if o.path == path => o }.get
          val em = new Emitter(fed.dag, p => readOf(p.head, p.drop(1)), indent)
          em.emitExprRoot(ow.value)
        case None => readExpr
      }
      if (muxPairs.isEmpty) {
        if (fallbackEntry.isDefined || dir == "out") out += s"$indent io.${param}Out.${path.drop(1).mkString(".")} := $rhs"
      } else {
        out += s"$indent io.${param}Out.${path.drop(1).mkString(".")} := MuxCase($rhs, Seq("
        out += s"$indent$indent ${muxPairs.mkString(", ")}))"
      }
    }
    out.toSeq
  }

  private def lowerConstKey(e: Expr, resolver: WidthResolver, tableName: String): (BigInt, Int) = {
    val b = new Ir.Builder
    val lowering = new ExprLowering(resolver, b)
    val (id, w) = lowering.lower(e, None, Map.empty)
    val dag = Passes.runAll(b.finish(Seq(OutputWrite(Seq("__k"), id, w))))
    dag.nodes(dag.outputs.head.asInstanceOf[OutputWrite].value) match {
      case Const(v, ww) => (v, ww)
      case _ => throw new P4Error(s"table '$tableName'：表项 key 必须是常量")
    }
  }

  private def combineKeys(vals: Seq[(BigInt, Int)], widths: Seq[Int]): BigInt = {
    var acc = BigInt(0)
    vals.zip(widths).foreach { case ((v, w), expected) =>
      if (w > expected) throw new P4Error("表项 key 值超出字段宽度")
      acc = (acc << expected) | (v & ((BigInt(1) << expected) - 1))
    }
    acc
  }

  private def lowerEntry(e: TableEntry, resolver: WidthResolver, c: ControlDecl, prog: P4Program, externMap: Map[String, ExternInst]): Ir.Dag = {
    val actions = c.actions.map(a => a.name -> a).toMap
    val a = actions.getOrElse(e.action, throw new P4Error(s"静态融合表项：未知 action '${e.action}'"))
    if (e.args.length != a.params.length)
      throw new P4Error(s"action '${e.action}' 表项实参数量不匹配")
    val b = new Ir.Builder
    val lowering = new ExprLowering(resolver, b, externMap)
    val binds: Map[String, (NodeId, Int)] = a.params.zip(e.args).map { case (p, arg) =>
      val (id, w) = lowering.lower(arg, Some(p.width), Map.empty)
      p.name -> ((b.fit(id, w, p.width), p.width))
    }.toMap
    val outs = a.body.map {
      case asg: Assign => lowering.lowerAssign(asg.path, asg.expr, binds)
      case mc: MethodCall => lowering.lowerMethodCall(mc, binds)
      case s => throw new P4Error(s"行 ${s.line}：action 体中不支持的语句")
    }
    Passes.runAll(b.finish(outs))
  }

  // ---------------- parser 发射（M3） ----------------

  private final case class StateLayout(
    extracts: Seq[(Seq[String], HeaderType, Int)], // 路径, header 类型, 字节偏移
    trans: TransStmt,
  )

  private def layoutParser(p: ParserDecl, prog: P4Program): Map[String, StateLayout] = {
    val headerTypes = prog.headerTypes.map(ht => ht.name -> ht).toMap
    val states = p.states.map(s => s.name -> s).toMap
    val layouts = mutable.HashMap.empty[String, StateLayout]

    def headerSize(ht: HeaderType): Int = ht.fields.map(f => (f.width + 7) / 8).sum

    def dfs(name: String, byteOffset: Int): Unit = {
      if (name == "accept" || name == "reject") return
      val st = states.getOrElse(name, throw new P4Error(s"parser：未知状态 '$name'"))
      layouts.get(name) match {
        case Some(existing) =>
          val curOff = existing.extracts.headOption.map(_._3).getOrElse(byteOffset)
          if (curOff != byteOffset)
            throw new P4Error(s"parser：状态 '$name' 在不同路径下字节偏移不同（${curOff} vs $byteOffset），M3 子集要求固定偏移")
        case None =>
          var off = byteOffset
          val extracts = mutable.ArrayBuffer.empty[(Seq[String], HeaderType, Int)]
          st.stmts.foreach {
            case Extract(path, ln) =>
              if (path.length != 2) throw new P4Error(s"行 $ln：extract 路径必须是 param.instance")
              val inst = path(1)
              // 找到 out 参数 struct 中该实例的类型
              val member = p.params.collectFirst {
                case cp if cp.name == path.head && cp.direction == "out" => (cp, prog.structs.find(_.name == cp.typeName))
              }.flatMap { case (cp, so) => so.flatMap(_.members.find(_.name == inst)) }
                .getOrElse(throw new P4Error(s"行 $ln：extract 目标 '$path' 未找到"))
              if (member.isBits) throw new P4Error(s"行 $ln：extract 目标必须是 header 实例")
              val ht = headerTypes.getOrElse(member.typeName, throw new P4Error(s"未知 header 类型 '${member.typeName}'"))
              extracts += ((path, ht, off))
              off += headerSize(ht)
            case _: Goto | _: Select =>
            case s => throw new P4Error(s"行 ${s.line}：parser 状态中不支持的语句")
          }
          layouts(name) = StateLayout(extracts.toSeq, st.stmts.collectFirst { case t: TransStmt => t }
            .getOrElse(throw new P4Error(s"parser 状态 '$name' 缺少 transition")))
          // 递归后继
          layouts(name).trans match {
            case Goto(t, _) => dfs(t, off)
            case Select(_, cases, deft, _) =>
              cases.foreach { case (_, t) => dfs(t, off) }
              dfs(deft, off)
          }
      }
    }
    dfs("start", 0)
    layouts.toMap
  }

  private def emitParser(prefix: String, p: ParserDecl, prog: P4Program, tmap: String => String): Seq[String] = {
    val out = mutable.ArrayBuffer.empty[String]
    val layouts = layoutParser(p, prog)
    val className = pascal(prefix) + pascal(p.name) + "Parser"
    val windowBits = 512

    // out 参数 bundle
    val outParams = p.params.filter(_.direction == "out")
    val bundleDefs = mutable.ArrayBuffer.empty[String]
    outParams.foreach { op =>
      val st = prog.structs.find(_.name == op.typeName).getOrElse(throw new P4Error(s"parser out 参数 '${op.name}' 类型不是 struct"))
      val bn = s"${prefix}_out_${op.name}"
      bundleDefs += s"final class $bn extends Bundle {"
      st.members.foreach {
        case StructMember(_, true, w, n) => bundleDefs += s"${IND}val $n = UInt($w.W)"
        case StructMember(tn, false, _, n) =>
          bundleDefs += s"${IND}val ${n}Valid = Bool()"
          bundleDefs += s"${IND}val $n = new ${tmap(tn)}"
      }
      bundleDefs += "}"
      bundleDefs += ""
    }

    // 状态枚举（保持声明顺序 + accept/reject）
    val stateNames = p.states.map(_.name).filter(n => n != "accept" && n != "reject") ++ Seq("accept", "reject")
    val enumNames = stateNames.map(n => s"s_$n")

    out ++= bundleDefs
    out += s"final class $className extends Module {"
    out += s"${IND}val io = IO(new Bundle {"
    out += s"$IND$IND val in = Input(UInt($windowBits.W))"
    outParams.foreach(op => out += s"$IND$IND val ${op.name}Out = Output(new ${prefix}_out_${op.name})")
    out += s"$IND$IND val done = Output(Bool())"
    out += s"$IND$IND val error = Output(Bool())"
    out += s"$IND})"
    out += s"$IND val ${enumNames.mkString(" :: ")} :: Nil = Enum(${enumNames.size})"
    out += s"$IND val state = RegInit(s_start)"
    out += s"$IND val done = RegInit(false.B)"
    out += s"$IND val error = RegInit(false.B)"
    outParams.foreach(op => out += s"$IND val r_${op.name} = RegInit(0.U.asTypeOf(new ${prefix}_out_${op.name}))")
    outParams.foreach(op => out += s"$IND io.${op.name}Out := r_${op.name}")
    out += s"$IND io.done := done"
    out += s"$IND io.error := error"
    out += ""
    out += s"$IND switch (state) {"

    // 供 select 引用的字段发射：返回 wire 名
    def extractStatements(path: Seq[String], ht: HeaderType, byteOff: Int, indent: String, paramName: String): Seq[String] = {
      val inst = path(1)
      val hb = ht.fields.map(_.width).sum
      if (hb % 8 != 0) throw new P4Error(s"header '${ht.name}' 总宽度 $hb bit 非字节对齐（M3 子集限制）")
      val shift = windowBits - 8 * byteOff - hb
      if (shift < 0) throw new P4Error(s"header '${ht.name}' 在偏移 $byteOff 超出 $windowBits-bit 报文窗口")
      val ls = mutable.ArrayBuffer.empty[String]
      ls += s"$indent val w_$inst = (io.in >> ${shift}.U)(${hb - 1}, 0)"
      var p = 0
      ht.fields.foreach { f =>
        ls += s"$indent r_$paramName.$inst.${f.name} := w_$inst(${hb - 1 - p}, ${hb - p - f.width})"
        p += f.width
      }
      ls += s"$indent r_$paramName.${inst}Valid := true.B"
      ls.toSeq
    }

    layouts.toSeq.sortBy { case (n, _) => stateNames.indexOf(n) }.foreach { case (name, layout) =>
      out += s"$IND$IND is (s_$name) {"
      layout.extracts.foreach { case (path, ht, off) =>
        out ++= extractStatements(path, ht, off, s"$IND$IND$IND", path.head)
      }
      layout.trans match {
        case Goto(t, _) =>
          if (t == "accept") out += s"$IND$IND$IND state := s_accept"
          else if (t == "reject") out += s"$IND$IND$IND state := s_reject"
          else out += s"$IND$IND$IND state := s_$t"
        case Select(value, cases, deft, _) =>
          // select 字段必须是本状态刚提取的字段
          val path = value match {
            case Name(pp, _) => pp
            case _ => throw new P4Error("M3 select 值必须是字段路径")
          }
          val ext = layout.extracts.find { e =>
            val (ep, eht2, _) = e
            ep.head == path.head && ep(1) == path(1) && eht2.fields.exists(_.name == path.lift(2).getOrElse(""))
          }.getOrElse(
            throw new P4Error(s"select 字段 '${path.mkString(".")}' 未在本状态提取"))
          val (_, eht, _) = ext
          val inst = path(1)
          val fname = path(2)
          val f = eht.fields.find(_.name == fname).getOrElse(throw new P4Error(s"header '${eht.name}' 无字段 '$fname'"))
          val hb = eht.fields.map(_.width).sum
          val p0 = eht.fields.takeWhile(_.name != fname).map(_.width).sum
          val w = f.width
          out += s"$IND$IND$IND val sel = w_$inst(${hb - 1 - p0}, ${hb - p0 - w})"
          val defTgt = if (deft == "accept") "s_accept" else if (deft == "reject") "s_reject" else s"s_$deft"
          // when/elsewhen/otherwise 链（switch 的 default 在 chisel3 3.6 不可用）
          cases.zipWithIndex.foreach { case ((ce, target), i) =>
            val (cv, cw) = constSelValue(ce, w, prog, p)
            val tgt = if (target == "accept") "s_accept" else if (target == "reject") "s_reject" else s"s_$target"
            val cond = s"sel === 0x${cv.toString(16)}.U($cw.W)"
            if (i == 0) out += s"$IND$IND$IND when ($cond) { state := $tgt }"
            else out += s"$IND$IND$IND .elsewhen ($cond) { state := $tgt }"
          }
          out += s"$IND$IND$IND .otherwise { state := $defTgt }"
      }
      out += s"$IND$IND }"
    }
    out += s"$IND$IND is (s_accept) { done := true.B }"
    out += s"$IND$IND is (s_reject) { error := true.B; done := true.B }"
    out += s"$IND }"
    out += "}"
    out.toSeq
  }

  private def constSelValue(e: Expr, w: Int, prog: P4Program, p: ParserDecl): (BigInt, Int) = {
    val resolver = new WidthResolver(
      prog.headerTypes.map(ht => ht.name -> ht).toMap,
      prog.structs.map(st => st.name -> st).toMap,
      p.params,
    )
    val b = new Ir.Builder
    val lowering = new ExprLowering(resolver, b)
    val (id, lw) = lowering.lower(e, Some(w), Map.empty)
    val dag = Passes.runAll(b.finish(Seq(OutputWrite(Seq("__s"), b.fit(id, lw, w), w))))
    dag.nodes(dag.outputs.head.asInstanceOf[OutputWrite].value) match {
      case Const(v, ww) => (v, ww)
      case _ => throw new P4Error("select 分支值必须是常量")
    }
  }

  // ---------------- 程序发射 ----------------

  /** 仅类型 Bundle（CLI 单文件模式用，原名发射）。 */
  def emitTypes(prog: P4Program): String = {
    val out = mutable.ArrayBuffer.empty[String]
    out += "// Generated by P4C (P4 → Chisel). DO NOT EDIT."
    out += "package p4cgen"
    out += ""
    out += "import chisel3._"
    out += "import chisel3.util._"
    out += ""
    out ++= emitBundles(prog, n => n)
    out.mkString("\n")
  }

  /** 仅模块（自带带前缀的 Bundle 定义）。 */
  def emitModules(prog: P4Program, moduleNamePrefix: String, sourceName: String): String = {
    val tmap0 = typeMapOf(prog, moduleNamePrefix)
    val tmap: String => String = n => tmap0.getOrElse(n, n)
    val out = mutable.ArrayBuffer.empty[String]
    out += s"// Generated by P4C (P4 → Chisel) from $sourceName. DO NOT EDIT."
    out += "package p4cgen"
    out += ""
    out += "import chisel3._"
    out += "import chisel3.util._"
    out += ""
    out ++= emitBundles(prog, tmap)
    prog.controls.foreach { c => out ++= emitControl(moduleNamePrefix, c, prog, tmap); out += "" }
    prog.parsers.foreach { p => out ++= emitParser(moduleNamePrefix, p, prog, tmap); out += "" }
    // M5：parser + control 同时存在时，发射管线 Top
    if (prog.parsers.nonEmpty && prog.controls.nonEmpty) {
      out ++= emitTop(moduleNamePrefix, prog.parsers.head, prog.controls.head, prog, tmap)
      out += ""
    }
    out.mkString("\n")
  }

  /** M5：管线 Top —— parser 解析后触发 control（一次性），输出锁存。 */
  private def emitTop(
    prefix: String, p: ParserDecl, c: ControlDecl, prog: P4Program, tmap: String => String,
  ): Seq[String] = {
    val out = mutable.ArrayBuffer.empty[String]
    val parserCls = pascal(prefix) + pascal(p.name) + "Parser"
    val controlCls = pascal(prefix) + pascal(c.name)
    val topCls = pascal(prefix) + "Top"
    val windowBits = 512

    // control 的第一个 inout/in 参数接 parser 输出（类型需匹配），其余参数清零
    val hdrParam = c.params.find(_.direction == "inout").orElse(c.params.find(_.direction == "in"))
      .getOrElse(throw new P4Error("Top 组装：control 无 inout/in 参数可接 parser 输出"))
    val parserOut = p.params.find(_.direction == "out")
      .getOrElse(throw new P4Error("Top 组装：parser 无 out 参数"))
    if (hdrParam.typeName != parserOut.typeName)
      throw new P4Error(s"Top 组装：parser 输出类型 '${parserOut.typeName}' 与 control 参数 '${hdrParam.typeName}' 不匹配")
    val zeroParams = c.params.filter(x => x != hdrParam && (x.direction == "inout" || x.direction == "in"))
    // parser 输出 struct 中的 header 成员（valid 位随解析置位）
    val hdrMembers = prog.structs.find(_.name == parserOut.typeName)
      .map(_.members.filterNot(_.isBits)).getOrElse(Seq.empty)

    out += s"final class $topCls extends Module {"
    out += s"${IND}val io = IO(new Bundle {"
    out += s"$IND$IND val in = Input(UInt($windowBits.W))"
    out += s"$IND$IND val outValid = Output(Bool())"
    out += s"$IND$IND val error = Output(Bool())"
    hdrMembers.foreach { m => out += s"$IND$IND val ${m.name}Valid = Output(Bool())" }
    c.externs.foreach { e => out += s"$IND$IND val ex_${e.name} = Output(Vec(${e.size}, UInt(${e.width}.W)))" }
    c.params.foreach {
      case ControlParam(n, "inout", t, _) => out += s"$IND$IND val ${n}Out = Output(new ${tmap(t)})"
      case ControlParam(n, "out", t, _) => out += s"$IND$IND val ${n}Out = Output(new ${tmap(t)})"
      case _ =>
    }
    out += s"$IND})"
    out += ""
    out += s"$IND val parser = Module(new $parserCls)"
    out += s"$IND val ingress = Module(new $controlCls)"
    out += ""
    out += s"$IND parser.io.in := io.in"
    out += s"$IND ingress.io.${hdrParam.name}In := parser.io.${parserOut.name}Out"
    zeroParams.foreach { zp =>
      out += s"$IND ingress.io.${zp.name}In := 0.U.asTypeOf(new ${tmap(zp.typeName)})"
    }
    hdrMembers.foreach { m =>
      out += s"$IND io.${m.name}Valid := parser.io.${parserOut.name}Out.${m.name}Valid"
    }
    c.externs.foreach { e =>
      out += s"$IND io.ex_${e.name} := ingress.io.ex_${e.name}"
    }
    // control 只在解析完成后执行一次
    out += s"$IND val fired = RegInit(false.B)"
    out += s"$IND val fire = parser.io.done && !parser.io.error && !fired"
    if (c.externs.nonEmpty) out += s"$IND ingress.io.valid := fire"
    out += s"$IND when (fire) { fired := true.B }"
    out += ""
    out += s"$IND io.outValid := RegNext(fire)"
    out += s"$IND io.error := parser.io.error"
    c.params.foreach { cp =>
      if (cp.direction == "inout" || cp.direction == "out")
        out += s"$IND io.${cp.name}Out := ingress.io.${cp.name}Out"
    }
    out += "}"
    out.toSeq
  }

  /** 单文件发射（CLI 模式：类型 + 模块合一）。 */
  def emitProgram(prog: P4Program, moduleNamePrefix: String, sourceName: String): String =
    emitTypes(prog) + "\n" + emitModules(prog, moduleNamePrefix, sourceName)

  /** 每个模块文件自带带前缀的 Bundle，避免多文件间类名冲突。 */
  def typeMapOf(prog: P4Program, prefix: String): Map[String, String] =
    (prog.headerTypes.map(ht => ht.name -> s"${pascal(prefix)}_${ht.name}") ++
      prog.structs.map(st => st.name -> s"${pascal(prefix)}_${st.name}")).toMap
}
