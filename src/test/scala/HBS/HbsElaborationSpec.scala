package HBS

import _root_.circt.stage.ChiselStage
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import chisel3._
import HBS.adm.HbsAdm
import HBS.swf.common.{BusMatrix, SwfParams, VoqBuffer}
import HBS.swf.sfu_routing.SfuRouting
import HBS.tm.{PacketLinkList, TmParam}
import HBS.top.HbsParams

/** HBS 迁入后的 elaboration 冒烟测试（轻量集）。
  *
  * 只做 elaboration（`emitCHIRRTL` 原生路径，不经过 firtool），与 `BaseCbb.RegCbb`
  * 既有约定一致，用于保证移植代码在 HD 底座上还能正确构建；不覆盖功能正确性。
  *
  * 未纳入本 spec 的重顶层（`SwfCore` / `SfuTop` / `SfuCorner` / `SfuMid`）：
  * 其实例化规模极大，elaboration 需要 >6GB 堆（实测 6GB 仍 OOM、GC 占比 115%），
  * 不适合放进默认 `sbt test`。需要时用 HBS 自带入口离线生成：
  * {{{ sbt -J-Xmx8G "runMain HBS.swf.SwfMain" }}}
  */
class HbsElaborationSpec extends AnyFlatSpec with Matchers {

  private def elaborate(name: String)(gen: => RawModule): Unit = {
    it should s"elaborate $name" in {
      ChiselStage.emitCHIRRTL(gen) should not be empty
    }
  }

  elaborate("HbsAdm")(new HbsAdm)
  elaborate("BusMatrix")(new BusMatrix(UInt(32.W), 12, 24))
  elaborate("VoqBuffer")(new VoqBuffer(BufferDep = 64, DataW = 32, QueueNum = 4))
  elaborate("PacketLinkList")(new PacketLinkList(new TmParam(new HbsParams)))
  elaborate("SfuRouting")(new SfuRouting(new SwfParams))
}
