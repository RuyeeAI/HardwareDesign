ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.ethanhau"

val chiselVersion = "3.6.1"

val p4Stages = settingKey[Int]("P4C 拍数预算（1 = 不切拍；env P4C_STAGES 覆盖）")
val p4Clock = settingKey[Int]("P4C 时钟约束（每级最大权重上限，自动搜最小可行级数；0 = 关闭；env P4C_CLOCK 覆盖）")
val p4DelayModel = settingKey[String]("P4C 延迟模型（weighted|unit|JSON 文件路径；env P4C_DELAY_MODEL 覆盖）")

lazy val root = (project in file("."))
  .settings(
    name := "HardwareDesign",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "org.scalatest" %% "scalatest" % "3.2.20" % "test",
      "edu.berkeley.cs" %% "chiseltest" % "0.6.2" % Test,
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
    // ---------- P4C：P4 → Chisel 编译器，demo 生成管线 ----------
    p4Stages := sys.env.getOrElse("P4C_STAGES", "1").toInt,
    p4Clock := sys.env.getOrElse("P4C_CLOCK", "0").toInt,
    p4DelayModel := sys.env.getOrElse("P4C_DELAY_MODEL", "weighted"),
    p4Generate := {
      val out = (Compile / sourceManaged).value / "p4c"
      val copyDir = baseDirectory.value / "generated" / "p4c"
      val sigDir = baseDirectory.value / "generated" / "p4c_signature"
      val demos = (baseDirectory.value / "p4" / "demos" * "*.p4").get
      P4C.Generate.generateAll(demos, out, Some(copyDir), p4Stages.value, streams.value.log.info(_),
        Some(sigDir), if (p4Clock.value > 0) Some(p4Clock.value) else None,
        P4C.DelayModels.load(p4DelayModel.value))
    },
    Compile / sourceGenerators += Def.task { p4Generate.value }.taskValue,
    // 切拍变体管线：p4/demos/staged/*.p4 -> <Prefix>Staged.scala（类名 +Staged 后缀）。
    // 拍数预算默认 4，可用 env P4C_STAGED_STAGES 覆盖（如 P4C_STAGED_STAGES=3 sbt compile）。
    p4GenerateStaged := {
      val out = (Compile / sourceManaged).value / "p4c"
      val copyDir = baseDirectory.value / "generated" / "p4c" / "staged"
      val demos = (baseDirectory.value / "p4" / "demos" / "staged" * "*.p4").get
      P4C.Generate.generateStagedVariants(
        demos, out, sys.env.getOrElse("P4C_STAGED_STAGES", "4").toInt,
        streams.value.log.info(_), Some(copyDir))
    },
    Compile / sourceGenerators += Def.task { p4GenerateStaged.value }.taskValue,
  )

lazy val p4Generate = taskKey[Seq[File]]("Compile p4/demos/*.p4 into generated Chisel sources")
lazy val p4GenerateStaged = taskKey[Seq[File]]("Compile p4/demos/staged/*.p4 into staged (multi-cycle) Chisel variants")
