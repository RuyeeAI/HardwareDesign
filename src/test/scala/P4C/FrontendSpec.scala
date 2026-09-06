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
}
