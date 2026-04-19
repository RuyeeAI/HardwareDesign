package BaseCbb.utils

import scala.io.{Source, StdIn}
object ReadFile{
  def apply(file:String):Seq[String]={
    val inputFile = Source.fromFile(file)
    val lines = inputFile.getLines
    lines.toSeq
  }
}

object WriteFile{
  /**
   * Write context to a file
   * @param file file name
   * @param context context to be written
   */
  def apply(file:String,context:String)={
    import java.io._
    val writer = new PrintWriter(new File(file))
    writer.write(context)
    writer.close()
  }
}

object ReadStdIO{
  def apply()={
    println("Please type in what you choice:")
    val line = StdIn.readLine()
    line
  }
}
//
//object JsonMapTool{
//  // Convert Map to Json
//  def apply(map:Map[Any,Any]):String = {
//
//  }
//
//  def apply(str:String):Map[Any,Any]={
//
//  }
//  // Conver
//}
