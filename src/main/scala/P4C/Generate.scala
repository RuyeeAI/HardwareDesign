package P4C

import P4C.Ast.P4Program

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** 编译驱动：.p4 文件 → 生成的 Chisel（Scala）源码。 */
object Generate {

  final case class Result(p4File: Path, scalaFile: Path, modules: Seq[String])

  /** 单文件编译：返回生成的 .scala 路径（同时可选拷贝一份到发布目录）。 */
  def compileFile(p4Path: Path, outDir: Path, copyDir: Option[Path]): Result = {
    val src = new String(Files.readAllBytes(p4Path), StandardCharsets.UTF_8)
    val prog = Parser.parseProgram(src)
    val stem = p4Path.getFileName.toString.stripSuffix(".p4")
    val prefix = stem.split("-|_").map(s => if (s.isEmpty) s else s.charAt(0).toUpper + s.substring(1)).mkString
    val modules =
      prog.controls.map(_.name) ++ prog.parsers.map(_ + "Parser")
    val code = ChiselBackend.emitProgram(prog, prefix, p4Path.getFileName.toString)

    Files.createDirectories(outDir)
    val out = outDir.resolve(s"$prefix.scala")
    Files.write(out, code.getBytes(StandardCharsets.UTF_8))
    copyDir.foreach { dir =>
      Files.createDirectories(dir)
      Files.write(dir.resolve(out.getFileName.toString), code.getBytes(StandardCharsets.UTF_8))
    }
    Result(p4Path, out, modules)
  }

  /** 批量编译（sbt sourceGenerators 入口）。每个生成文件自带带前缀的 Bundle。返回生成的托管源文件列表。 */
  def generateAll(files: Seq[java.io.File], outDir: java.io.File, copyDir: Option[java.io.File], log: String => Unit): Seq[java.io.File] = {
    val outs = scala.collection.mutable.ArrayBuffer.empty[java.io.File]
    Files.createDirectories(outDir.toPath)
    files.sortBy(_.getName).foreach { f =>
      try {
        val src = new String(Files.readAllBytes(f.toPath), StandardCharsets.UTF_8)
        val prog = Parser.parseProgram(src)
        val stem = f.getName.stripSuffix(".p4")
        val prefix = stem.split("-|_").map(s => if (s.isEmpty) s else s.charAt(0).toUpper + s.substring(1)).mkString
        val out = outDir.toPath.resolve(s"$prefix.scala")
        Files.write(out, ChiselBackend.emitModules(prog, prefix, f.getName).getBytes(StandardCharsets.UTF_8))
        copyDir.foreach { d =>
          Files.createDirectories(d.toPath)
          Files.write(d.toPath.resolve(out.getFileName.toString), Files.readAllBytes(out))
        }
        val modules = prog.controls.map(_.name) ++ prog.parsers.map(_.name + "Parser")
        log(s"[P4C] ${f.getName} -> ${out.getFileName} (modules: ${modules.mkString(", ")})")
        outs += out.toFile
      } catch {
        case e: P4Error => log(s"[P4C] 编译失败 ${f.getName}: ${e.getMessage}")
      }
    }
    outs.toSeq
  }
}

/** 命令行入口：P4cMain <in.p4> <outDir> [copyDir] */
object P4cMain {
  def main(args: Array[String]): Unit = args.toList match {
    case in :: out :: copyOpt =>
      val r = Generate.compileFile(Paths.get(in), Paths.get(out), copyOpt.headOption.map(Paths.get(_)))
      println(s"[P4C] ${r.p4File} -> ${r.scalaFile} (modules: ${r.modules.mkString(", ")})")
    case _ =>
      System.err.println("用法: P4cMain <in.p4> <outDir> [copyDir]")
      System.exit(1)
  }
}
