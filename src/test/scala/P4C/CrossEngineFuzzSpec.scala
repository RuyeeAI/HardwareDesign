package P4C

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** X4：交叉引擎 fuzzer（对标 XLS fuzzer 思路）——同一程序两引擎执行并比对：
  *   ① 黄金引擎：P4C 前端 AST → IrBuilder 降级 → [[Interp]]（IR 解释器）求值；
  *   ② 被测引擎：chiseltest 驱动生成的 Chisel 模块（p4cgen）。
  *
  * 覆盖：demo1（直行 action）/ demo2（静态表）/ demo7（静态+运行时表，随机写入表项）。
  * 范围限定：组合 control 模块（无 extern 顺序语义、无 parser/Top）——后续立项扩展。
  * 随机源固定 seed，保证可复现。
  */
class CrossEngineFuzzSpec extends AnyFreeSpec with ChiselScalatestTester with Matchers {

  private val Rounds = 20
  private val rng = new scala.util.Random(20260906L)

  private def read(p: String): String =
    new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(p)),
      java.nio.charset.StandardCharsets.UTF_8)

  // ---------------- io 展平与 IR 路径映射 ----------------

  /** 递归展平聚合 → (点分路径段, UInt 叶子)。 */
  private def flatten(d: chisel3.Data, prefix: Seq[String]): Seq[(Seq[String], chisel3.UInt)] = d match {
    case u: chisel3.UInt => Seq((prefix, u))
    case v: chisel3.Vec[_] => v.zipWithIndex.flatMap { case (e, i) => flatten(e, prefix :+ i.toString) }.toSeq
    case b: chisel3.Bundle => b.elements.toSeq.flatMap { case (n, e) => flatten(e, prefix :+ n) }
    case _ => Seq.empty
  }

  /** io 端口名 → IR 路径：首段去 In/Out 后缀（hdrIn.ethernet.etherType → hdr.ethernet.etherType）。 */
  private def irPath(segs: Seq[String]): Seq[String] =
    segs.patch(0, Seq(segs.head.stripSuffix("In").stripSuffix("Out")), 1)

  private def mask(w: Int): BigInt = (BigInt(1) << w) - 1

  /** 随机驱动全部输入，回读实际值作为黄金引擎输入（两侧同源）。 */
  private def randomizeInputs(io: chisel3.Data): Map[Seq[String], BigInt] = {
    val els = flatten(io, Seq.empty).filter { case (p, _) =>
      p.head.endsWith("In") && !p.head.startsWith("tbl_")
    }
    els.foreach { case (_, u) =>
      u.poke((BigInt(u.getWidth, rng) & mask(u.getWidth)).U(u.getWidth.W))
    }
    els.map { case (p, u) => irPath(p) -> u.peek().litValue }.toMap
  }

  /** 被测引擎输出比对：黄金缺项 = 输入透传（bulk connect 语义）。 */
  private def checkOutputs(
    io: chisel3.Data, inputs: Map[Seq[String], BigInt],
    goldenOut: Map[Seq[String], BigInt], tag: String,
  ): Unit = {
    val outputs = flatten(io, Seq.empty).filter { case (p, _) => p.head.endsWith("Out") }
    outputs.foreach { case (p, u) =>
      val ip = irPath(p)
      val expected = goldenOut.getOrElse(ip, inputs.getOrElse(ip,
        fail(s"$tag：黄金输出缺路径 '${p.mkString(".")}'")))
      withClue(s"$tag 路径 '${p.mkString(".")}'") {
        u.expect((expected & mask(u.getWidth)).U(u.getWidth.W))
      }
    }
  }

  // ---------------- 黄金引擎（AST → IrBuilder → Interp） ----------------

  private def resolverFor(prog: Ast.P4Program, c: Ast.ControlDecl): IrBuilder.WidthResolver =
    new IrBuilder.WidthResolver(
      prog.headerTypes.map(ht => ht.name -> ht).toMap,
      prog.structs.map(st => st.name -> st).toMap,
      c.params)

  /** action 体求值：binds 已含形参绑定（实参 DAG 或运行时常量）。 */
  private def evalBody(
    prog: Ast.P4Program, c: Ast.ControlDecl, a: Ast.ActionDecl,
    binds: IrBuilder.Bindings, bld: Ir.Builder,
    inputs: Map[Seq[String], BigInt],
  ): Interp.Result = {
    val lowering = new IrBuilder.ExprLowering(resolverFor(prog, c), bld)
    val outs = a.body.map {
      case asg: Ast.Assign => lowering.lowerAssign(asg.path, asg.expr, binds)
      case mc: Ast.MethodCall => lowering.lowerMethodCall(mc, binds)
      case s => fail(s"黄金引擎：action 体不支持语句 $s")
    }
    Interp.eval(Passes.runAll(bld.finish(outs)), Interp.Env(inputs = inputs))
  }

  /** 实参为 AST 表达式（apply 直呼 / 表项 args）。 */
  private def evalAction(
    prog: Ast.P4Program, c: Ast.ControlDecl, a: Ast.ActionDecl, args: Seq[Ast.Expr],
    inputs: Map[Seq[String], BigInt],
  ): Interp.Result = {
    val bld = new Ir.Builder
    val lowering = new IrBuilder.ExprLowering(resolverFor(prog, c), bld)
    val binds: IrBuilder.Bindings = a.params.zip(args).map { case (p, e) =>
      val (id, w) = lowering.lower(e, Some(p.width), Map.empty)
      p.name -> ((bld.fit(id, w, p.width), p.width))
    }.toMap
    evalBody(prog, c, a, binds, bld, inputs)
  }

  /** 实参为运行时值（运行时表条目 args 位串切出的参数值）。 */
  private def evalActionValues(
    prog: Ast.P4Program, c: Ast.ControlDecl, a: Ast.ActionDecl, values: Seq[BigInt],
    inputs: Map[Seq[String], BigInt],
  ): Interp.Result = {
    val bld = new Ir.Builder
    val binds: IrBuilder.Bindings = a.params.zip(values).map { case (p, v) =>
      p.name -> ((bld.add(Ir.Const(v & mask(p.width), p.width)), p.width))
    }.toMap
    evalBody(prog, c, a, binds, bld, inputs)
  }

  /** 表项 key 常量合成（按 key 声明位宽拼接，先声明在高位）。 */
  private def entryKey(
    prog: Ast.P4Program, c: Ast.ControlDecl, e: Ast.TableEntry, keyWidths: Seq[Int],
  ): BigInt =
    e.keys.zip(keyWidths).foldLeft(BigInt(0)) { case (acc, (expr, w)) =>
      val b = new Ir.Builder
      val lowering = new IrBuilder.ExprLowering(resolverFor(prog, c), b)
      val (id, kw) = lowering.lower(expr, None, Map.empty)
      val dag = Passes.runAll(b.finish(Seq(Ir.OutputWrite(Seq("__k"), id, kw))))
      val v = Interp.eval(dag, Interp.Env()).outputs(Seq("__k"))
      (acc << w) | (v & mask(w))
    }

  /** 静态表黄金查找：key 匹配（声明序优先）→ 命中 action；否则 default；都无 → 透传。 */
  private def staticTableResult(
    prog: Ast.P4Program, c: Ast.ControlDecl, t: Ast.TableDecl,
    inputs: Map[Seq[String], BigInt],
  ): Interp.Result = {
    val resolver = resolverFor(prog, c)
    val actions = c.actions.map(a => a.name -> a).toMap
    val keyWidths = t.keys.map { k =>
      k.expr match {
        case Ast.Name(p, _) => resolver.widthOf(p)
        case o => fail(s"黄金引擎：key 非路径 $o")
      }
    }
    val keyVal = t.keys.zip(keyWidths).foldLeft(BigInt(0)) { case (acc, (k, w)) =>
      val Ast.Name(p, _) = k.expr
      (acc << w) | (inputs(p) & mask(w))
    }
    val matched = t.entries.filterNot(_.isDefault).find(e => entryKey(prog, c, e, keyWidths) == keyVal)
    matched match {
      case Some(e) => evalAction(prog, c, actions(e.action), e.args, inputs)
      case None =>
        t.entries.find(_.isDefault)
          .map(d => evalAction(prog, c, actions(d.action), d.args, inputs))
          .getOrElse(Interp.Result())
    }
  }

  // ---------------- 用例 ----------------

  "fuzz demo1（直行 action）IR 解释器 vs 生成 RTL" in {
    val prog = Parser.parseProgram(read("p4/demos/demo1-action.p4"))
    val c = prog.controls.head
    val Ast.ActionCall(name, args, _) = c.applyBody.head
    val action = c.actions.find(_.name == name).get
    test(new Demo1ActionIngress) { c2 =>
      for (round <- 1 to Rounds) {
        val inputs = randomizeInputs(c2.io)
        val res = evalAction(prog, c, action, args, inputs)
        checkOutputs(c2.io, inputs, inputs ++ res.outputs, s"demo1 round $round")
      }
    }
  }

  "fuzz demo2（静态表）IR 解释器 vs 生成 RTL" in {
    val prog = Parser.parseProgram(read("p4/demos/demo2-match.p4"))
    val c = prog.controls.head
    test(new Demo2MatchIngress) { c2 =>
      for (round <- 1 to Rounds) {
        val inputs = randomizeInputs(c2.io)
        val res = staticTableResult(prog, c, c.tables.head, inputs)
        checkOutputs(c2.io, inputs, inputs ++ res.outputs, s"demo2 round $round")
      }
    }
  }

  "fuzz demo7（静态 + 运行时表，随机表项）IR 解释器 vs 生成 RTL" in {
    val prog = Parser.parseProgram(read("p4/demos/demo7-runtime-table.p4"))
    val c = prog.controls.head
    val actions = c.actions.map(a => a.name -> a).toMap
    val EntryW = 43
    // 运行时表条目编码（与 Demo7RuntimeTableSpec 一致）：valid(42)|act(41:40)|args(39:16)|key(15:0)
    def entry(act: Int, key: BigInt, args: BigInt, valid: Boolean): BigInt =
      (if (valid) BigInt(1) << (EntryW - 1) else BigInt(0)) |
        (BigInt(act) << 40) | ((args & mask(24)) << 16) | (key & mask(16))
    test(new Demo7RuntimeTableIngress) { c2 =>
      for (round <- 1 to Rounds) {
        // 随机表项写入口（地址 0..2，50% 有效；act 随机 0/1/2，key/args 随机）
        val entries = (0 until 3).map { addr =>
          val valid = rng.nextBoolean()
          val act = rng.nextInt(3)
          val key = BigInt(16, rng)
          val args = act match {
            case 0 => BigInt(8, rng)  // set_cls(c)：c 占 args 低 8 位
            case 1 => BigInt(24, rng) // set_port(p, t)：p 占 [23:8]、t 占 [7:0]
            case _ => BigInt(0)       // nop 无参数
          }
          c2.io.tbl_rt_table_we.poke(true.B)
          c2.io.tbl_rt_table_waddr.poke(addr.U(3.W))
          c2.io.tbl_rt_table_wdata.poke(entry(act, key, args, valid).U(EntryW.W))
          c2.clock.step(1)
          (addr, valid, act, key, args)
        }
        c2.io.tbl_rt_table_we.poke(false.B)

        val inputs = randomizeInputs(c2.io)
        // 黄金查找：低地址优先，首个 valid && key 匹配
        val etherType = inputs(Seq("hdr", "ethernet", "etherType"))
        val golden = entries.find { case (_, valid, _, key, _) => valid && key == etherType } match {
          case Some((_, _, 0, _, args)) =>
            evalActionValues(prog, c, actions("set_cls"), Seq(args & mask(8)), inputs)
          case Some((_, _, 1, _, args)) =>
            evalActionValues(prog, c, actions("set_port"), Seq((args >> 8) & mask(16), args & mask(8)), inputs)
          case _ => Interp.Result() // nop / miss → default nop → 透传
        }
        val staticRes = staticTableResult(prog, c, c.tables.find(_.name == "static_table").get, inputs)
        // 两表写字段不相交（stat vs cls/normPort/tag），合并顺序无关
        checkOutputs(c2.io, inputs, inputs ++ staticRes.outputs ++ golden.outputs, s"demo7 round $round")
      }
    }
  }
}
