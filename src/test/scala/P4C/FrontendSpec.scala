package P4C

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** 前端 + IR 优化 pass 单元测试。 */
class FrontendSpec extends AnyFreeSpec with Matchers {

  private def parse(src: String) = Parser.parseProgram(src)

  "词法器应支持带宽字面量" in {
    val toks = Lexer.tokenize(Preprocess("16w0x0800 8w3 0x86dd 255"))
    def check(i: Int, text: String, v: BigInt, w: Option[Int]) = {
      toks(i).kind shouldBe Lexer.TNum
      toks(i).text shouldBe text
      toks(i).value shouldBe Some(v)
      toks(i).width shouldBe w
    }
    check(0, "16w0x0800", 0x800, Some(16))
    check(1, "8w3", 3, Some(8))
    check(2, "0x86dd", 0x86dd, None)
    check(3, "255", 255, None)
  }

  "预处理应去掉注释与 # 行" in {
    val out = Preprocess("// c\n#include <x>\nbit<8> a; /* block */ bit<8> b;")
    out.trim shouldBe "bit<8> a;  bit<8> b;"
  }

  "应解析 M1 demo（header/struct/control/action）" in {
    val src = new String(java.nio.file.Files.readAllBytes(
      java.nio.file.Paths.get("p4/demos/demo1-action.p4")))
    val prog = parse(src)
    prog.headerTypes should have size 1
    prog.structs should have size 2
    prog.controls should have size 1
    val c = prog.controls.head
    c.actions.head.params should have size 1
    c.applyBody should have size 1
  }

  "应解析 M2 demo 的 table 与 const entries" in {
    val src = new String(java.nio.file.Files.readAllBytes(
      java.nio.file.Paths.get("p4/demos/demo2-match.p4")))
    val prog = parse(src)
    val t = prog.controls.head.tables.head
    t.name shouldBe "cls_table"
    t.keys should have size 1
    t.entries should have size 3
    t.entries.last.isDefault shouldBe true
    t.entries.head.args should have size 1
  }

  "应解析 M3 demo 的 parser 状态机" in {
    val src = new String(java.nio.file.Files.readAllBytes(
      java.nio.file.Paths.get("p4/demos/demo3-parser.p4")))
    val prog = parse(src)
    prog.parsers should have size 1
    val p = prog.parsers.head
    p.states.map(_.name) shouldBe Seq("start", "parse_ethernet", "parse_ipv4")
  }

  "表达式宽度推断：移位取左操作数宽度" in {
    val prog = parse(
      """header h { bit<16> f; }
        |struct s { h hh; }
        |control C(inout s x, inout s y) {
        |  action a() { x.hh.f = (y.hh.f << 3) | 8w1; }
        |  apply { a(); }
        |}""".stripMargin)
    prog.controls should have size 1
  }

  "字面量超出自身声明宽度应报错（赋值截断则允许）" in {
    def compile(src: String) = {
      val dir = java.nio.file.Files.createTempDirectory("p4c-test")
      Generate.compileFile(java.nio.file.Paths.get(
        java.nio.file.Files.write(dir.resolve("t.p4"), src.getBytes).toString), dir, None)
    }
    // 16w0x12345 超出 16 位声明宽度 → IR 构建时报错
    an[P4Error] should be thrownBy compile(
      """header h { bit<8> f; }
        |struct s { h hh; }
        |control C(inout s x, inout s y) {
        |  action a() { x.hh.f = 16w0x12345; }
        |  apply { a(); }
        |}""".stripMargin)
    // 16w0x1234 赋给 8 位字段：P4 语义允许隐式截断，不报错
    compile(
      """header h { bit<8> f; }
        |struct s { h hh; }
        |control C(inout s x, inout s y) {
        |  action a() { x.hh.f = 16w0x1234; }
        |  apply { a(); }
        |}""".stripMargin)
  }

  /** 运行时表模板：`entries` 段为空 = 不写 const entries。 */
  private def runtimeTableSrc(entries: String): String =
    s"""header h { bit<16> f; }
       |struct s { h hh; }
       |control C(inout s x, inout s y) {
       |  action a(bit<8> p) { x.hh.f = p; }
       |  action nop() { }
       |  // p4c: table t runtime size=4
       |  table t {
       |    key = { x.hh.f : exact; }
       |    actions = { a; nop; }
       |$entries  }
       |  apply { t.apply(); }
       |}""".stripMargin

  "运行时表不允许非 default 的 const entries（报错含表名）" in {
    def compile(src: String) = {
      val dir = java.nio.file.Files.createTempDirectory("p4c-test")
      Generate.compileFile(java.nio.file.Paths.get(
        java.nio.file.Files.write(dir.resolve("t.p4"), src.getBytes).toString), dir, None)
    }
    val e = intercept[P4Error] {
      compile(runtimeTableSrc("    const entries = { 0x0800 : a(8w1); default : nop(); }\n"))
    }
    e.getMessage should include("运行时表")
    e.getMessage should include("'t'")
    e.getMessage should include("const entries")
  }

  "运行时表可省略 default（全 miss 时字段透传，编译通过）" in {
    def compile(src: String) = {
      val dir = java.nio.file.Files.createTempDirectory("p4c-test")
      val r = Generate.compileFile(java.nio.file.Paths.get(
        java.nio.file.Files.write(dir.resolve("t.p4"), src.getBytes).toString), dir, None)
      new String(java.nio.file.Files.readAllBytes(r.scalaFile), java.nio.charset.StandardCharsets.UTF_8)
    }
    val code = compile(runtimeTableSrc(""))
    code should include("val rt_t = RegInit(VecInit(Seq.fill(4)(0.U")
    code should include("tbl_t_we")
    code should include("rt_t_hits")
  }

  "运行时表暂不支持切拍（--stages > 1）：明确报错而非静默生成" in {
    val dir = java.nio.file.Files.createTempDirectory("p4c-test")
    val f = java.nio.file.Files.write(dir.resolve("t.p4"), runtimeTableSrc("").getBytes)
    val e = intercept[P4Error] { Generate.compileFile(f, dir, None, 3) }
    e.getMessage should include("暂不支持切拍")
    e.getMessage should include("'t'")
  }
}

/** IR 优化 pass 单元测试。 */
class IrPassSpec extends AnyFreeSpec with Matchers {
  import P4C.Ir._

  "常量折叠：Const 运算直接求值" in {
    val b = new Builder
    val c1 = b.add(Const(0x0800, 16))
    val c2 = b.add(Const(0x00ff, 16))
    val x = b.add(Bin(Xor, c1, c2, 16))
    val dag = b.finish(Seq(OutputWrite(Seq("f"), x, 16)))
    val opt = Passes.runAll(dag)
    opt.nodes(opt.outputs.head.asInstanceOf[OutputWrite].value) shouldBe Const(0x8ff, 16)
  }

  "常量折叠：Zext/Trunc/Slice/Mux 常量传播" in {
    val b = new Builder
    val c = b.add(Const(0xabcd, 16))
    val s = b.add(Slice(c, 15, 8))
    val dag = b.finish(Seq(OutputWrite(Seq("f"), s, 8)))
    val opt = Passes.runAll(dag)
    opt.nodes(opt.outputs.head.asInstanceOf[OutputWrite].value) shouldBe Const(0xab, 8)
  }

  "CSE：相同子表达式只算一次" in {
    val b = new Builder
    val i = b.add(InputRef(Seq("x", "f"), 16))
    val a1 = b.add(Bin(Add, i, b.add(Const(1, 16)), 16))
    val a2 = b.add(Bin(Add, i, b.add(Const(1, 16)), 16))
    val dag = b.finish(Seq(
      OutputWrite(Seq("o", "p"), a1, 16),
      OutputWrite(Seq("o", "q"), a2, 16)))
    val opt = Passes.runAll(dag)
    opt.outputs(0).asInstanceOf[OutputWrite].value shouldBe opt.outputs(1).asInstanceOf[OutputWrite].value
  }

  "DCE：不可达节点被删除" in {
    val b = new Builder
    val used = b.add(Const(7, 8))
    val dead = b.add(Bin(Add, b.add(Const(1, 8)), b.add(Const(2, 8)), 8))
    val dag = b.finish(Seq(OutputWrite(Seq("f"), used, 8)))
    val opt = Passes.runAll(dag)
    opt.nodes.count(_.isInstanceOf[Bin]) shouldBe 0
    opt.nodes.size should be < dag.nodes.size
  }

  // ---------------- X5：语义简化 pass（simplify） ----------------

  "slice-of-concat：对齐切片直接命中 part" in {
    val b = new Builder
    val a = b.add(InputRef(Seq("x", "a"), 8))
    val c = b.add(InputRef(Seq("x", "c"), 8))
    val cat = b.add(Cat(Seq(a, c), 16)) // a [15:8]、c [7:0]
    val sl = b.add(Slice(cat, 15, 8))
    val opt = Passes.runAll(b.finish(Seq(OutputWrite(Seq("f"), sl, 8))))
    opt.nodes(opt.outputs.head.asInstanceOf[OutputWrite].value) shouldBe InputRef(Seq("x", "a"), 8)
  }

  "slice-of-concat：跨 part 切片重建为子 Slice 的 Cat" in {
    val b = new Builder
    val a = b.add(InputRef(Seq("x", "a"), 8))
    val c = b.add(InputRef(Seq("x", "c"), 8))
    val cat = b.add(Cat(Seq(a, c), 16))
    val sl = b.add(Slice(cat, 11, 4)) // a 高 4 位 ++ c 高 4 位
    val opt = Passes.runAll(b.finish(Seq(OutputWrite(Seq("f"), sl, 8))))
    val Cat(parts, 8) = opt.nodes(opt.outputs.head.asInstanceOf[OutputWrite].value)
    val Seq(na, nc) = parts.map(id => opt.nodes(id))
    na match {
      case Ir.Slice(ai, 3, 0) => opt.nodes(ai) shouldBe InputRef(Seq("x", "a"), 8) // a[15:8] ∩ [11:4] = a[3:0]
      case other => fail(s"期望 Slice，got $other")
    }
    nc match {
      case Ir.Slice(ci, 7, 4) => opt.nodes(ci) shouldBe InputRef(Seq("x", "c"), 8) // c[7:0] ∩ [11:4] = c[7:4]
      case other => fail(s"期望 Slice，got $other")
    }
  }

  "布尔恒等：And(x, 全1)=x、Or(x, 全1)=全1、Xor(x, 全1)=Not(x)" in {
    val b = new Builder
    val x = (b.add(InputRef(Seq("m", "x"), 8)), 8)
    val ones = (b.add(Const(0xff, 8)), 8)
    val andN = b.bin(And, x, ones)
    val orN = b.bin(Or, x, ones)
    val xorN = b.bin(Xor, x, ones)
    val opt = Passes.runAll(b.finish(Seq(
      OutputWrite(Seq("f0"), andN._1, 8),
      OutputWrite(Seq("f1"), orN._1, 8),
      OutputWrite(Seq("f2"), xorN._1, 8))))
    opt.nodes(opt.outputs(0).asInstanceOf[OutputWrite].value) shouldBe InputRef(Seq("m", "x"), 8)
    opt.nodes(opt.outputs(1).asInstanceOf[OutputWrite].value) shouldBe Const(0xff, 8)
    opt.nodes(opt.outputs(2).asInstanceOf[OutputWrite].value) match {
      case Not(s, 8) => opt.nodes(s) shouldBe InputRef(Seq("m", "x"), 8)
      case other => fail(s"期望 Not，got $other")
    }
  }

  "Not(Not(x)) = x" in {
    val b = new Builder
    val x = b.add(InputRef(Seq("m", "x"), 8))
    val n1 = b.add(Not(x, 8))
    val n2 = b.add(Not(n1, 8))
    val opt = Passes.runAll(b.finish(Seq(OutputWrite(Seq("f"), n2, 8))))
    opt.nodes(opt.outputs.head.asInstanceOf[OutputWrite].value) shouldBe InputRef(Seq("m", "x"), 8)
  }

  "自反消除：Sub(x,x)=0、Xor(x,x)=0、And(x,x)=x" in {
    val b = new Builder
    val x = (b.add(InputRef(Seq("m", "x"), 8)), 8)
    val sub = b.bin(Sub, x, x)
    val xor = b.bin(Xor, x, x)
    val and = b.bin(And, x, x)
    val opt = Passes.runAll(b.finish(Seq(
      OutputWrite(Seq("f0"), sub._1, 8),
      OutputWrite(Seq("f1"), xor._1, 8),
      OutputWrite(Seq("f2"), and._1, 8))))
    opt.nodes(opt.outputs(0).asInstanceOf[OutputWrite].value) shouldBe Const(0, 8)
    opt.nodes(opt.outputs(1).asInstanceOf[OutputWrite].value) shouldBe Const(0, 8)
    opt.nodes(opt.outputs(2).asInstanceOf[OutputWrite].value) shouldBe InputRef(Seq("m", "x"), 8)
  }

  "simplify 属性检查：优化前后 DAG 对随机输入求值一致（Interp 交叉验证）" in {
    val rng = new scala.util.Random(42)
    val b = new Builder
    val x = (b.add(InputRef(Seq("m", "x"), 16)), 16)
    val y = (b.add(InputRef(Seq("m", "y"), 16)), 16)
    val cat = b.add(Cat(Seq(x._1, y._1), 32))
    val sl = b.add(Slice(cat, 23, 8))
    val add = b.bin(Add, x, y)
    val xor = b.bin(Xor, (sl, 16), (b.add(Const(0xffff, 16)), 16))
    val sub = b.bin(Sub, add, add)
    val dag = b.finish(Seq(
      OutputWrite(Seq("f0"), sl, 16),
      OutputWrite(Seq("f1"), xor._1, 16),
      OutputWrite(Seq("f2"), sub._1, 16)))
    val opt = Passes.runAll(dag)
    for (_ <- 0 until 50) {
      val env = Interp.Env(inputs = Map(
        Seq("m", "x") -> BigInt(16, rng),
        Seq("m", "y") -> BigInt(16, rng)))
      val r0 = Interp.eval(dag, env)
      val r1 = Interp.eval(opt, env)
      r0.outputs shouldBe r1.outputs
    }
    // 结构效果：x+x 自反、全 1 异或、cat 切片各至少削减一层
    opt.nodes.size should be < dag.nodes.size
  }
}
