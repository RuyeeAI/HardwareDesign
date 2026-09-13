ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.ethanhau"

// 与 topdesign_dpsk / TopDesign 对齐：Chisel 5.3.0（org.chipsalliance）。
// 仿真统一走 chisel3.simulator（随 chisel 提供），不再依赖已停止维护的 chiseltest。
val chiselVersion = "5.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "HardwareDesign",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "org.scalatest" %% "scalatest" % "3.2.20" % "test",
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
    ),
    addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full),
  )
