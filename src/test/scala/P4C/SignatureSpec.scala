package P4C

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets

/** X1：签名/调度 JSON 导出单测（对标 XLS signature/schedule textproto）。 */
class SignatureSpec extends AnyFlatSpec with Matchers {

  /** 编译并返回（生成代码文本, 签名 JSON 文本）。 */
  private def gen(src: String, stages: Int): (String, String) = {
    val dir = java.nio.file.Files.createTempDirectory("p4c-sig")
    val f = java.nio.file.Files.write(dir.resolve("t.p4"), src.getBytes)
    val sigDir = dir.resolve("sig")
    val r = Generate.compileFile(f, dir, None, stages, Some(sigDir))
    val code = new String(java.nio.file.Files.readAllBytes(r.scalaFile), StandardCharsets.UTF_8)
    val json = new String(
      java.nio.file.Files.readAllBytes(sigDir.resolve("T.json")), StandardCharsets.UTF_8)
    (code, json)
  }

  private val src =
    """header h { bit<16> f; bit<8> g; }
      |struct s { h hh; bit<8> tag; }
      |control C(inout s x, inout s y) {
      |  action bump() { y.hh.f = x.hh.f + 16w1; }
      |  action a(bit<8> p) { y.hh.g = p; y.tag = 8w1; }
      |  action nop() { }
      |  // p4c: table t runtime size=4
      |  table t {
      |    key = { x.hh.f : exact; }
      |    actions = { a; nop; }
      |    const entries = { default : nop(); }
      |  }
      |  apply { bump(); t.apply(); }
      |}""".stripMargin

  behavior.of("签名 JSON（N=1，默认模式）")

  private val (code1, json1) = gen(src, stages = 1)

  it should "包含模块名、展平端口（含运行时表写口）与表布局" in {
    json1 should include(""""module":"TC"""")
    // inout 参数展开为 In/Out 两组叶子端口
    json1 should include(""""path":"xIn.hh.f","dir":"input","width":16""")
    json1 should include(""""path":"xOut.tag","dir":"output","width":8""")
    // 运行时表写端口（addrW=2, entryW=1+1+8+16=26）
    json1 should include(""""path":"tbl_t_we","dir":"input","width":1""")
    json1 should include(""""path":"tbl_t_waddr","dir":"input","width":2""")
    json1 should include(""""path":"tbl_t_wdata","dir":"input","width":26""")
    // 表清单：运行时表带布局回显；静态信息不含布局字段
    json1 should include(""""name":"t","runtime":true,"size":4,"keyBits":16,"actW":1,"argW":8,"entryW":26,"addrW":2""")
    json1 should not include """"runtime":false"""
    json1 should not include """"valid""""
    // 无 extern
    json1 should include(""""externs":[]""")
    // 生成代码不受影响（签名是旁路产物）
    code1 should include("final class TC extends Module")
  }

  it should "未调度 DAG 全部记 stage=0、stageCount=1" in {
    json1 should include(""""ctx":"rt table t/nop","stageCount":1""")
    json1 should include(""""op":"InputRef","width":16,"stage":0""")
  }

  behavior.of("签名 JSON（切拍模式）")

  /** 切拍用例不含运行时表（运行时表本期仅 N=1，见 Q4 裁定）。 */
  private val stagedSrc =
    """header h { bit<16> f; bit<8> g; }
      |struct s { h hh; bit<8> tag; }
      |control C(inout s x, inout s y) {
      |  action bump() { y.hh.f = x.hh.f + 16w1; }
      |  apply { bump(); }
      |}""".stripMargin

  it should "调度后 DAG 导出节点 → 流水级映射" in {
    val (_, json) = gen(stagedSrc, stages = 3)
    // bump：单个 Add（W=1）→ n=min(3, 2)=2
    json should include(""""ctx":"control C/action bump","stageCount":2""")
    json should include(""""op":"Bin(Add)","width":16,"stage":1""")
  }
}
