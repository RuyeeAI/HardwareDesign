package BaseCbb.utils

import org.json4s._
import org.json4s.native._
import org.json4s.native.JsonMethods._

import scala.collection.mutable.ArrayBuffer

object JsonTools{
  def saveAsPrettyJsonFile(fileName:String,json:String)={
    import org.json4s.native.JsonMethods._
    val prettyJsonString = pretty(render(parse(json)))
    WriteFile(fileName,prettyJsonString)
  }

  def jsonStr2Map(json:String):Map[String,Any]={
    implicit  val formats:DefaultFormats.type = org.json4s.DefaultFormats
    parse(json).extract[Map[String,Any]]
  }

  def jsonStr2List(json:String):Seq[Any]={
    implicit  val formats:DefaultFormats.type = org.json4s.DefaultFormats
    parse(json).extract[Seq[Any]]
  }

  def scalaTypetoJson(query:Any):String = query match{
    case m:Map[String,Any] => s"{${m.map(scalaTypetoJson(_)).mkString(",")}}"
    case t:(String,Any) => s""""${t._1}":${scalaTypetoJson(t._2)}"""
    case ss:Seq[Any] => s"""[${ss.map(scalaTypetoJson).mkString(",")}]"""
    case aa:ArrayBuffer[Any] => s"""[${aa.map(scalaTypetoJson).mkString(",")}]"""
    case s:String => val b = s.replaceAll("\n","")
      s""""$b""""
    case e:Enumeration => s""""${e.toString()}""""
    case null =>"null"
    case _ => s""""${query.toString}""""
  }

  def saveMap2JsonFile(fileName:String,query:Any)={
    saveAsPrettyJsonFile(fileName,scalaTypetoJson(query))
  }

}