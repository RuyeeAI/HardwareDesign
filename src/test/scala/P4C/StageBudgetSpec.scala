package P4C

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** 拍数预算参数面验证（QA 补充，覆盖工程师未写的角度）：
  * staged 变体类的 budget 在构建期固定（P4C_STAGED_STAGES=4），无法在测试期针对
  * 不同 budget 编译出可仿真的硬件类；本文件退而在**生成代码文本层**验证不同预算
  * （2 vs 8 vs 1）的切拍结构正确性：
  *   - budget=N>1：sV 链长恰为 N（demo6 加法链深度 D=15 ≥ N，不降级），链为
  *     RegNext 纯延迟线（valid 链语义），outValid 指向末级，状态写门控恰一次；
  *   - budget=1（默认模式）：无任何切拍产物（D4 零切拍结构 + D5：有 extern 仍发射
  *     valid 端口但不发射 outValid）；
  *   - budget<1：P4Error。
  */
class StageBudgetSpec extends AnyFlatSpec with Matchers {

  private val demo6 = Paths.get("p4/demos/demo6-deepchain.p4")

  private def gen(stages: Int): String = {
    val tmp = Files.createTempDirectory(s"p4c-budget$stages")
    try {
      val r = Generate.compileFile(demo6, tmp, None, stages)
      new String(Files.readAllBytes(r.scalaFile), StandardCharsets.UTF_8)
    } finally {
      // 保留目录供失败时排查；CI 临时目录会被系统回收
      ()
    }
  }

  private def countContains(code: String, pat: String): Int =
    pat.r.findAllMatchIn(code).length

  behavior.of("Generate.compileFile（拍数预算参数面）")

  it should "budget=2：sV 链长 2、RegNext 延迟线、outValid := sV_1、状态写门控恰一次" in {
    val code = gen(2)
    code should include("val sV_0 = io.valid")
    code should include("val sV_1 = RegNext(sV_0, false.B)")
    countContains(code, "RegNext\\(sV_") should be(1) // 链长 2 ⇒ 1 级 RegNext
    code should not include "sV_2"
    code should include("io.outValid := sV_1")
    countContains(code, "when \\(sV_1\\)") should be(1) // 所有状态写共享单一末级门控
    code should include("val valid = Input(Bool())")   // D5
    code should include("val outValid = Output(Bool())") // D4
    // 边界寄存器用 RegEnable（数据），与 valid 链的 RegNext（纯延迟线）区分
    countContains(code, "RegEnable\\(") should be > 0
  }

  it should "budget=8：sV 链长 8（D=15 未降级）、outValid := sV_7" in {
    val code = gen(8)
    (0 until 8).foreach(k => code should include(s"val sV_$k ="))
    countContains(code, "RegNext\\(sV_") should be(7)
    code should not include "sV_8"
    code should include("io.outValid := sV_7")
    countContains(code, "when \\(sV_7\\)") should be(1)
  }

  it should "budget=2 与 budget=8 对同一 demo 切分粒度不同但结构性质一致" in {
    val c2 = gen(2)
    val c8 = gen(8)
    // 一致性：valid 链起点、末级门控结构、outValid 契约、D5 端口
    Seq(c2, c8).foreach { code =>
      code should include("val sV_0 = io.valid")
      code should include("val valid = Input(Bool())")
      code should include("val outValid = Output(Bool())")
      code should include("when (sV_")
      code should include("RegEnable(")
    }
    // 差异性：链长 = budget（加法链 D=15 深于两个预算，均不触发降级）
    countContains(c2, "RegNext\\(sV_") should be(1)
    countContains(c8, "RegNext\\(sV_") should be(7)
    // 两者代码不同（budget 生效）
    c2 should not be c8
  }

  it should "budget=1：无切拍产物（无 sV/无 outValid/无边界寄存器），有 extern 故仍发射 valid 端口" in {
    val code = gen(1)
    code should not include "sV_"
    code should not include "outValid"
    code should not include "RegEnable("
    code should not include "RegNext("
    code should not include "val v_" // 无切拍边界寄存器
    code should include("val valid = Input(Bool())") // D5：stateful ⇒ N=1 也发射
    code should include("when (io.valid)") // N=1 原路径 fire 门控
  }

  it should "budget<1 抛 P4Error" in {
    an[P4Error] should be thrownBy gen(0)
    an[P4Error] should be thrownBy gen(-3)
  }
}
