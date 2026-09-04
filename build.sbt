ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.ethanhau"

val chiselVersion = "3.6.1"

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
    p4Generate := {
      val out = (Compile / sourceManaged).value / "p4c"
      val copyDir = baseDirectory.value / "generated" / "p4c"
      val demos = (baseDirectory.value / "p4" / "demos" * "*.p4").get
      P4C.Generate.generateAll(demos, out, Some(copyDir), streams.value.log.info(_))
    },
    Compile / sourceGenerators += Def.task { p4Generate.value }.taskValue,
  )

lazy val p4Generate = taskKey[Seq[File]]("Compile p4/demos/*.p4 into generated Chisel sources")
