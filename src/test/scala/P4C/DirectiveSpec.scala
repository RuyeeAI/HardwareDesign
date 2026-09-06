package P4C

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** E2：声明级切拍编译指示（`// p4c: stages=N`）单测。
  *
  * 覆盖：指示解析（大小写/空格容忍、尾巴忽略）、非法值 P4Error、紧邻性校验
  * （策略：隔了代码/注释行 → 忽略 + 告警）、优先级（声明级覆盖全局）。
  */
class DirectiveSpec extends AnyFlatSpec with Matchers {

  private def parse(src: String) = Parser.parseProgramWithDiagnostics(src)

  behavior.of("Directive.scan（指示行扫描）")

  it should "识别标准指示：// p4c: stages=N" in {
    val s = Directive.scan("// p4c: stages=3\ncontrol C(){}")
    s.directives should be(Map(1 -> 3))
  }

  it should "忽略大小写与空格差异，数字后尾巴内容忽略" in {
    val s = Directive.scan(
      """//P4C:STAGES=2
        |  //   p4c  :  stages  =  7   (本行尾巴忽略)
        |control C(){}""".stripMargin)
    s.directives should be(Map(1 -> 2, 2 -> 7))
  }

  it should "普通注释与无指示源码不产生指示" in {
    Directive.scan("// p4c stage=2\n// p4c stages:\n// hello p4c: stages=3").directives shouldBe empty
  }

  it should "N=0 或负数/无法解析的指示 → P4Error（带行号）" in {
    val e0 = intercept[P4Error] { Directive.scan("x\n// p4c: stages=0") }
    e0.getMessage should include("行 2")
    e0.getMessage should include("≥ 1")
    val eNeg = intercept[P4Error] { Directive.scan("// p4c: stages=-1") }
    eNeg.getMessage should include("行 1")
    val eBad = intercept[P4Error] { Directive.scan("// p4c: stages=abc") }
    eBad.getMessage should include("行 1")
    eBad.getMessage should include("无法解析")
  }

  behavior.of("Parser 声明级指示（紧邻性 + 优先级）")

  private val structDecl =
    """struct m_t { bit<16> f0; bit<16> f1; bit<16> f2; bit<16> f3; bit<16> f4; bit<16> f5; bit<16> acc; }"""

  it should "control 紧邻指示 → ControlDecl.stagesOpt = Some(N)" in {
    val (prog, _) = parse(
      s"""$structDecl
         |// p4c: stages=2
         |control Fast(inout m_t m) { apply { } }
         |control Slow(inout m_t m) { apply { } }""".stripMargin)
    prog.controls(0).stagesOpt should be(Some(2))
    prog.controls(1).stagesOpt should be(None)
  }

  it should "指示与声明之间允许空行，仍算紧邻" in {
    val (prog, _) = parse(
      s"""$structDecl
         |// p4c: stages=3
         |
         |
         |control C(inout m_t m) { apply { } }""".stripMargin)
    prog.controls.head.stagesOpt should be(Some(3))
  }

  it should "action 紧邻指示 → ActionDecl.stagesOpt = Some(N)，不影响同 control 其他 action" in {
    val (prog, _) = parse(
      s"""$structDecl
         |control C(inout m_t m) {
         |  // p4c: stages=2
         |  action a() { m.acc = m.f0 + m.f1; }
         |  action b() { m.acc = m.f2 + m.f3; }
         |  apply { a(); b(); }
         |}""".stripMargin)
    val c = prog.controls.head
    c.actions(0).stagesOpt should be(Some(2))
    c.actions(1).stagesOpt should be(None)
    c.stagesOpt should be(None) // action 指示不作用于 control 整体
  }

  it should "隔了代码行 → 忽略 + 告警（宽容策略，固化）" in {
    val (prog, warnings) = parse(
      s"""$structDecl
         |// p4c: stages=2
         |bit<8> dead; // 无效代码行隔开 → 指示不紧邻
         |control C(inout m_t m) { apply { } }""".stripMargin)
    prog.controls.head.stagesOpt should be(None)
    warnings.size should be(1)
    warnings.head should include("行 2")
    warnings.head should include("已忽略")
  }

  it should "隔了其他注释行 → 同样不紧邻，忽略 + 告警" in {
    val (prog, warnings) = parse(
      s"""$structDecl
         |// p4c: stages=2
         |// 普通注释行
         |control C(inout m_t m) { apply { } }""".stripMargin)
    prog.controls.head.stagesOpt should be(None)
    warnings should have size 1
  }

  it should "指示紧邻 header 声明 → 忽略 + 告警（仅 control/parser/action 认领）" in {
    val (_, warnings) = parse(
      """// p4c: stages=2
        |header h { bit<8> f; }
        |struct m_t { bit<16> acc; }
        |control C(inout m_t m) { apply { } }""".stripMargin)
    warnings should have size 1
  }

  it should "parser 紧邻指示 → ParserDecl.stagesOpt 记录（当前不生效，仅记录）" in {
    val (prog, _) = parse(
      """struct m_t { bit<8> f; }
        |// p4c: stages=2
        |parser P(packet_in b, out m_t m) {
        |  state start { transition accept; }
        |}""".stripMargin)
    prog.parsers.head.stagesOpt should be(Some(2))
  }

  it should "块注释跨行后指示仍按原始行号匹配（行号不变量）" in {
    val (prog, _) = parse(
      s"""$structDecl
         |/* 跨行
         |   块注释 */
         |// p4c: stages=5
         |control C(inout m_t m) { apply { } }""".stripMargin)
    prog.controls.head.stagesOpt should be(Some(5))
  }

  behavior.of("运行时表指示 // p4c: table <表名> runtime [size=N]")

  /** 含两张表（t1/t2）的 control 模板：`%RT%` 占位符处插入 table 指示行。 */
  private def twoTableSrc(rtDirective: String): String = {
    val dir = if (rtDirective.isEmpty) "" else rtDirective + "\n"
    s"""$structDecl
       |control C(inout m_t m) {
       |  action a() { m.acc = m.f0 + m.f1; }
       |  table t1 {
       |    key = { m.f0 : exact; }
       |    actions = { a; }
       |    const entries = { default : a(); }
       |  }
       |$dir  table t2 {
       |    key = { m.f1 : exact; }
       |    actions = { a; }
       |    const entries = { default : a(); }
       |  }
       |  apply { t1.apply(); t2.apply(); }
       |}""".stripMargin
  }

  it should "扫描识别运行时表指示，缺省表深 4" in {
    val s = Directive.scan("// p4c: table cls_table runtime\ncontrol C(){}")
    s.tableDirectives should be(Map(1 -> Directive.TableDirective("cls_table", 4)))
    s.directives shouldBe empty
  }

  it should "解析 size=N，且容忍大小写/空格差异与行尾尾巴" in {
    val s = Directive.scan(
      """//P4C:TABLE  t_a  RUNTIME  SIZE=8
        |//   p4c : table t_b runtime size = 3   （本行尾巴忽略）
        |control C(){}""".stripMargin)
    s.tableDirectives should be(Map(1 -> Directive.TableDirective("t_a", 8),
      2 -> Directive.TableDirective("t_b", 3)))
  }

  it should "size=0 与语法错误 → P4Error（带行号）" in {
    val e0 = intercept[P4Error] { Directive.scan("// p4c: table t runtime size=0") }
    e0.getMessage should include("行 1")
    e0.getMessage should include("size 必须 ≥ 1")
    val eBad = intercept[P4Error] { Directive.scan("// p4c: table t") }
    eBad.getMessage should include("行 1")
    eBad.getMessage should include("无法解析")
  }

  it should "table 声明紧邻指示 → TableDecl(isRuntime=true, runtimeSize=N)，且不影响同 control 其他表" in {
    val (prog, warnings) = parse(twoTableSrc("  // p4c: table t2 runtime size=6"))
    prog.controls.head.tables(0).isRuntime should be(false)
    prog.controls.head.tables(1).isRuntime should be(true)
    prog.controls.head.tables(1).runtimeSize should be(6)
    warnings shouldBe empty
  }

  it should "指示与 table 之间允许空行，仍算紧邻" in {
    val (prog, _) = parse(twoTableSrc("  // p4c: table t2 runtime\n\n"))
    prog.controls.head.tables(1).runtimeSize should be(4)
  }

  it should "指示表名与声明名不一致 → P4Error（信息含两个表名）" in {
    val e = intercept[P4Error] { parse(twoTableSrc("  // p4c: table t9 runtime")) }
    e.getMessage should include("t9")
    e.getMessage should include("t2")
    e.getMessage should include("不一致")
  }

  it should "隔了代码行 → 忽略 + 告警（与 stages 指示同一宽容策略）" in {
    val src = twoTableSrc("  // p4c: table t2 runtime").replace(
      "  // p4c: table t2 runtime\n  table t2 {", "  // p4c: table t2 runtime\n  bit<8> gap;\n  table t2 {")
    val (prog, warnings) = parse(src)
    prog.controls.head.tables(1).isRuntime should be(false)
    warnings.size should be(1)
    warnings.head should include("行 9")
    warnings.head should include("table t2 runtime")
    warnings.head should include("已忽略")
  }

  it should "与 stages 指示正交：control 上 stages=2 与 table 上 runtime 同时生效" in {
    val dir = "  // p4c: table t2 runtime size=2"
    val src = twoTableSrc(dir).replace("control C(inout m_t m) {", "// p4c: stages=2\ncontrol C(inout m_t m) {")
    val (prog, warnings) = parse(src)
    prog.controls.head.stagesOpt should be(Some(2))
    prog.controls.head.tables(1).isRuntime should be(true)
    warnings shouldBe empty
  }

  behavior.of("声明级指示优先级（端到端：Generate.compileFile）")

  /** 临时 .p4 编译并返回生成代码文本。 */
  private def gen(src: String, globalStages: Int): String = {
    val dir = java.nio.file.Files.createTempDirectory("p4c-directive")
    val f = java.nio.file.Files.write(dir.resolve("t.p4"), src.getBytes)
    val r = Generate.compileFile(f, dir, None, globalStages)
    new String(java.nio.file.Files.readAllBytes(r.scalaFile),
      java.nio.charset.StandardCharsets.UTF_8)
  }

  private val deepSrc =
    s"""$structDecl
       |// p4c: stages=2
       |control Fast(inout m_t m) {
       |  action chain() {
       |    m.acc = m.f0 + m.f1 + m.f2 + m.f3 + m.f4 + m.f5;
       |  }
       |  apply { chain(); }
       |}""".stripMargin

  it should "指示 stages=2 覆盖全局 8：Fast 恒 2 级（1 条 RegNext）" in {
    val code = gen(deepSrc, globalStages = 8)
    code should include("val sV_0 = io.valid")
    code should include("val sV_1 = RegNext(sV_0, false.B)")
    code should not include "sV_2"
    "RegNext\\(sV_".r.findAllMatchIn(code).size should be(1)
    code should include("io.outValid := sV_1")
  }

  it should "无指示同一源码走全局预算（对照：全局 4 → 3 条 RegNext）" in {
    val noDirective = deepSrc.replace("// p4c: stages=2\n", "")
    val code = gen(noDirective, globalStages = 4)
    "RegNext\\(sV_".r.findAllMatchIn(code).size should be(3) // W=5 → n=min(4,6)=4
    code should include("io.outValid := sV_3")
  }

  it should "全局 1 且无指示：与切拍前历史行为一致（零切拍产物）" in {
    val noDirective = deepSrc.replace("// p4c: stages=2\n", "")
    val code = gen(noDirective, globalStages = 1)
    code should not include "sV_"
    code should not include "outValid"
    code should not include "RegEnable("
  }
}
