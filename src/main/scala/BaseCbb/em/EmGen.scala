package em

import _root_.circt.stage.{ChiselStage, FirtoolOption}
import chisel3.stage.ChiselGeneratorAnnotation

// ===========================================================================
// EM 模块的 Verilog 产出入口
//
//   sbt "runMain em.EmGen <outDir> <preset>"
//
// 流程（chisel 7）：ChiselStage 发射 CHIRRTL（.fir，可读中间表示）→ ChiselStage 走
// circt 流水线出 SystemVerilog。firtool 版本由 chisel 钉死（7.15.0 ↔ firtool 1.158.0），
// 经 firtool-resolver 自动下载并缓存（~/.cache/circt 或 ~/Library/Caches/...）；
// 要用自带版本就设环境变量 CHISEL_FIRTOOL_PATH 指向包含 firtool 的目录。
// ⚠️ PATH 上的旧 firtool（如 1.62.0）解析不了 chisel7 的 CHIRRTL（新 layer 语法），
//    所以这里**不再手动调 PATH firtool**，一律走 chisel 的自动解析。
//
// ⚠️ 产出的 Verilog 用 **Verilator** 仿真（`verilator --binary --timing --trace ...`）。
//   **iverilog 编不过**：AgeTable 这类寄存器阵列（Vec）被 firtool 展成"连续赋值里对
//   数组做变量下标读"，iverilog 要求那里必须是常量下标（实测 11 个 elaboration error）。
//   firtool 的 `disallowLocalVariables` 仍然要带：否则函数内变量生成 `automatic`，
//   iverilog/部分工具链会报 unsupported。
// ===========================================================================
object EmGen {

  /** 预设配置：覆盖"全特性 / 各级裁剪 / CRC 两种形态 / OVFC"的组合 */
  def preset(name: String): EmParams = name match {
    case "basic" => // 三级齐全、无学习无老化
      EmParams(keyWidth = 48, adWidth = 32, htDepth = 1024, htWays = 4,
        numBanks = 1, ktDepth = 4096, adDepth = 1024)

    case "2left" => // d-left(2) + 自学习 + 老化
      EmParams(keyWidth = 48, adWidth = 32, htDepth = 1024, htWays = 4,
        numBanks = 2, dLeftTie = TiePolicy.Random, ktDepth = 8192, adDepth = 2048,
        aging = Some(AgingParams(ageWidth = 16, timeout = 1024, tickDiv = 64)),
        learning = Some(LearningParams(defaultAd = 0, usePortAd = true)))

    case "inline" => // 单级：key 与动作数据都内联进 HT（最快）
      EmParams(keyWidth = 48, adWidth = 32, htDepth = 1024, htWays = 4,
        numBanks = 1, useKt = false, useAd = false)

    case "noad" => // 两级：AD 内联进 KT
      EmParams(keyWidth = 48, adWidth = 32, htDepth = 1024, htWays = 4,
        numBanks = 1, ktDepth = 4096, useAd = false)

    case "crcrt" => // 运行时可配 CRC（串行 LFSR）
      EmParams(keyWidth = 48, adWidth = 32, htDepth = 1024, htWays = 4,
        numBanks = 1, ktDepth = 4096, adDepth = 1024, crc = CrcRuntime.crc32)

    case "tb" => // 功能仿真用小尺寸：HT 打得满，才能仿真 OVFC 溢出路径
      // 容量：HT = 32 桶 x 2 路 x 2 子表 = 64 条；OVFC = 8 条 → 总容量 72
      // 并列仲裁用 RoundRobin：结果确定，便于断言
      EmParams(keyWidth = 16, adWidth = 26, htDepth = 32, htWays = 2,
        numBanks = 2, dLeftTie = TiePolicy.RoundRobin, ktDepth = 128, adDepth = 128,
        ovfcDepth = 8,
        aging = Some(AgingParams(ageWidth = 8, timeout = 4, tickDiv = 8)),
        learning = Some(LearningParams(defaultAd = 0, usePortAd = true)))

    case other => sys.error(s"未知 preset: ${other}（可选：basic / 2left / inline / noad / crcrt / tb）")
  }

  def main(args: Array[String]): Unit = {
    val outDir = if (args.length > 0) args(0) else "out/a3"
    val name   = if (args.length > 1) args(1) else "basic"
    val p      = preset(name)
    val l      = EmLayout(p)

    println(s"[em] preset=$name")
    println(f"[em]   key=${l.keyW} ad=${l.adW} ht=${p.htDepth}x${l.ways} banks=${l.numBanks} " +
      f"kt=${p.ktDepth}(单实例,全局指针) ad=${p.adDepth} ovfc=${p.ovfcDepth}")
    println(s"[em]   useKt=${p.useKt} useAd=${p.useAd} crc=${p.crc.getClass.getSimpleName} " +
      s"aging=${p.aging.isDefined} learning=${p.learning.isDefined} protect=${p.memProtect}")
    println(s"[em]   HT负载=${l.htPayW}b（指纹 ${l.fpW}b + KT指针 ${l.ktPtrW}b）  KT条目=${l.ktEntryW}b  查找延迟=${l.lookupLatency}拍（II=1，CrcHardwired 下）")
    if (l.agingOn) {
      println(s"[em]   老化：valid/claim 寄存器阵列 ${l.ageWords}x${l.ageRegW}b（${l.ageWords * l.ageRegW} flop）" +
        s" + 时间戳 SRAM ${l.ageTsDepth}x${l.ageW}b（${l.ageTsDepth * l.ageW} bit，rdLat=1，不占查找带宽）")
    } else {
      println(s"[em]   老化：关闭（valid/claim 寄存器阵列仍存在：${l.ageWords}x${l.ageRegW}b）")
    }
    println(s"[em]   存储读延时=${l.rdLat}拍（flopIn=${p.memFlopIn} flopOut=${p.memFlopOut} " +
      s"CheckIn=${p.memCheckIn} CheckOut=${p.memCheckOut}）")

    val dir = s"$outDir/$name"
    // 1) CHIRRTL（供人工审查 / 第三方工具链）
    (new ChiselStage).execute(
      Array("--target-dir", dir, "--target", "chirrtl"),
      Seq(ChiselGeneratorAnnotation(() => new ExactMatch(p))))
    // 2) firtool → SystemVerilog（单文件 ExactMatch.sv；firtool 由 chisel 自动解析到 1.158.0）
    // disallowLocalVariables：不要把函数内变量生成为 `automatic` —— iverilog 对
    // "Overriding the default variable lifetime" 视为 unsupported（sorry），
    // 会以非零码退出、**不产出 vvp**。
    (new ChiselStage).execute(
      Array("--target-dir", dir, "--target", "systemverilog"),
      Seq(ChiselGeneratorAnnotation(() => new ExactMatch(p)),
          FirtoolOption("--disable-all-randomization"),
          FirtoolOption("--strip-debug-info"),
          FirtoolOption("--lowering-options=disallowLocalVariables")))
    val veri = s"$dir/ExactMatch.sv"
    if (!new java.io.File(veri).exists()) sys.error(s"firtool 未产出 $veri")
    println(s"[em] 产出：$veri")
  }
}
