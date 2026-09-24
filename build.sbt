ThisBuild / scalaVersion     := "2.13.16"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.ethanhau"

// Chisel 7.15.0（org.chipsalliance）：svsim 的 Verilator 后端自此支持 FST 波形
// （TraceKind.Fst，chisel 5.3/6.x 只有 VCD）。仿真统一走 chisel3.simulator。
val chiselVersion = "7.15.0"

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
