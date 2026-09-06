package P4C

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

/** QA 补充验证（严过关 · 第二层独立验证）：指示与块注释混合的行号边界、
  * 孤儿指示告警端到端（经 Generate.compileFile 真实编译通道）。
  *
  * 背景：Directive.scan 在**原始源码**上按行扫描（依赖 Preprocess 的行号不变量），
  * 触发规则是"任何以 // p4c: 起始的行"。本组用例覆盖两类边界：
  *   1. 块注释内部出现指示样文本（注释掉的指示）——不应生效、不应阻断编译；
  *   2. 孤儿指示告警走 compileFile 的 stdout 通道（端到端，非仅 parse 层）。
  */
class QaDirectiveEdgeSpec extends AnyFlatSpec with Matchers {

  private def parse(src: String) = Parser.parseProgramWithDiagnostics(src)

  private val structDecl =
    """struct m_t { bit<16> f0; bit<16> f1; bit<16> acc; }"""

  /** 捕获 Generate.compileFile 期间的 stdout（CLI 通道的告警走 Scala println →
    * Console.out 动态变量；必须用 Console.withOut 而非 System.setOut——Scala 的
    * println 在初始化时绑定 Console.out，System.setOut 不生效）。 */
  private def genCapturingStdout(src: String, globalStages: Int): (String, String) = {
    val dir = java.nio.file.Files.createTempDirectory("p4c-qa-edge")
    val f = java.nio.file.Files.write(dir.resolve("t.p4"), src.getBytes)
    val stdout = new ByteArrayOutputStream()
    val r = Console.withOut(stdout) {
      Generate.compileFile(f, dir, None, globalStages)
    }
    val code = new String(java.nio.file.Files.readAllBytes(r.scalaFile),
      StandardCharsets.UTF_8)
    (code, new String(stdout.toByteArray, StandardCharsets.UTF_8))
  }

  behavior.of("指示与块注释混合行号")

  it should "块注释整行包裹的指示样文本不生效：注释闭合行后紧跟声明的合法指示不受影响" in {
    // 行 2-3 是块注释；行 3 内部的 '// p4c: stages=3' 是纯文本。
    // 行 5 是真正紧邻指示的声明（行 4 空白）。
    val (prog, warnings) = parse(
      s"""$structDecl
         |/* debug note
         |// p4c: stages=3 */
         |
         |// p4c: stages=2
         |control C(inout m_t m) { apply { } }""".stripMargin)
    // 真指示（行 5）紧邻 → 生效；块注释内的伪指示（行 3）不应干扰
    prog.controls.head.stagesOpt should be(Some(2))
  }

  // 已修复（工程师，Directive.scan 接入 Preprocess.classify 共享注释状态机）：
  // 块注释内的 '// p4c:' 样文本被识别为"注释掉的指示"——不生效、不阻断编译、
  // 产生忽略告警。以下两条由 ignore 恢复为 it（QA 固化的回归用例）。
  it should "块注释闭合在指示行上：该指示是纯注释文本，不得被后续声明认领" in {
    // 行 3 起为块注释，行 4 = "// p4c: stages=3 */" 位于块注释内部。
    // 正确语义：这是注释掉的指示，不应生效（stagesOpt = None），且应有
    // "块注释内"专属忽略告警（非孤儿告警）。
    val (prog, warnings) = parse(
      s"""$structDecl
         |control A(inout m_t m) { apply { } }
         |/* 已弃用：暂不切拍
         |// p4c: stages=3 */
         |
         |control B(inout m_t m) { apply { } }""".stripMargin)
    prog.controls(1).stagesOpt should be(None)
    // 伪指示被 classify 判为块注释内 → suppressed 专属告警（不计入孤儿）
    warnings.exists(w => w.contains("行 4") && w.contains("块注释内") && w.contains("已忽略")) should be(true)
  }

  it should "块注释闭合后同行跟指示样文本：行首锚定下不触发指示语义（语法要求指示独占行首）" in {
    // 行 2 = "/* x */ // p4c: stages=2" —— triggerRe 行首锚定 ^\s*//，
    // 以 /* 开头的行从不触发指示（修复前后行为一致：静默、无告警）。
    val (prog, warnings) = parse(
      s"""$structDecl
         |/* x */ // p4c: stages=2
         |control C(inout m_t m) { apply { } }""".stripMargin)
    prog.controls.head.stagesOpt should be(None)
    warnings shouldBe empty
  }

  it should "指示样文本出现在代码行尾（非行首）不触发指示语义（无字符串字面量路径的边界对照）" in {
    // P4 子集无字符串字面量（Lexer 不识别引号，会词法报错），因此"指示出现在
    // 字符串里"路径不存在；同源边界 = 代码后跟指示样文本：triggerRe 行首锚定
    // → 不触发、不告警（静默，符合"指示必须独占一行"的语法）。
    // 用可直接词法的载体：预处理行内含指示样文本（整行剥除，语义为纯文本）。
    val (prog, warnings) = parse(
      s"""#define NOTE "// p4c: stages=9"
         |$structDecl
         |control C(inout m_t m) { apply { } }""".stripMargin)
    prog.controls.head.stagesOpt should be(None)
    warnings shouldBe empty // # 行整行按预处理剥除，不触发指示、不产生告警
  }

  it should "注释掉的非法指示不应阻断编译（P4Error 不应从注释内部抛出）" in {
    // 注释掉的历史指示残留（stages=abc）在块注释里，编译必须照常通过
    val (prog, _) = parse(
      s"""$structDecl
         |/*
         |// p4c: stages=abc
         |// p4c: stages=0
         |*/
         |control C(inout m_t m) { apply { } }""".stripMargin)
    prog.controls should have size 1
    prog.controls.head.stagesOpt should be(None)
  }

  behavior.of("运行时表指示与块注释混合")

  it should "块注释内的 table 指示不生效、不阻断编译（含注释掉的非法 size=0）" in {
    val (prog, warnings) = parse(
      s"""$structDecl
         |control C(inout m_t m) {
         |  action a() { m.acc = m.f0 + m.f1; }
         |  /* 已弃用：先不做运行时表
         |  // p4c: table t1 runtime size=9
         |  // p4c: table t1 runtime size=0
         |  */
         |  table t1 {
         |    key = { m.f0 : exact; }
         |    actions = { a; }
         |    const entries = { default : a(); }
         |  }
         |  apply { t1.apply(); }
         |}""".stripMargin)
    prog.controls.head.tables.head.isRuntime should be(false)
    warnings.count(w => w.contains("块注释内") && w.contains("已忽略")) should be(2)
    warnings should not contain "孤儿"
  }

  behavior.of("孤儿指示告警端到端（Generate.compileFile stdout 通道）")

  it should "孤儿指示经真实编译通道输出告警，且生成代码不受影响（走全局预算）" in {
    val src =
      s"""$structDecl
         |// p4c: stages=2
         |bit<8> gap; // 代码行隔开 → 孤儿指示
         |control Fast(inout m_t m) {
         |  action chain() { m.acc = m.f0 + m.f1; }
         |  apply { chain(); }
         |}""".stripMargin
    val (code, out) = genCapturingStdout(src, globalStages = 4)
    out should include("警告")
    out should include("已忽略")
    // 未生效 → 无 directive 生效日志
    out should not include "(directive)"
    // 走全局预算 4：W=1 → n=min(4,2)=2，outValid := sV_1
    code should include("io.outValid := sV_1")
    code should not include "sV_2"
  }

  it should "生效的指示不产生孤儿告警，且有 directive 生效日志（对照）" in {
    val src =
      s"""$structDecl
         |// p4c: stages=2
         |control Fast(inout m_t m) {
         |  action chain() { m.acc = m.f0 + m.f1; }
         |  apply { chain(); }
         |}""".stripMargin
    val (_, out) = genCapturingStdout(src, globalStages = 4)
    out should not include "已忽略"
    out should include("control Fast stages=2 (directive)")
  }
}
