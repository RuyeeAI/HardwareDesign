ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.ethanhau"

val chiselVersion = "3.6.1"

val p4Stages = settingKey[Int]("P4C 拍数预算（1 = 不切拍；env P4C_STAGES 覆盖）")

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
    p4Generate := {
      val out = (Compile / sourceManaged).value / "p4c"
      val copyDir = baseDirectory.value / "generated" / "p4c"
      val demos = (baseDirectory.value / "p4" / "demos" * "*.p4").get
      P4C.Generate.generateAll(demos, out, Some(copyDir), p4Stages.value, streams.value.log.info(_))
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
