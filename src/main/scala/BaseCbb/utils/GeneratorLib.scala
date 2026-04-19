package BaseCbb

import BaseCbb.MemoryAccessType.MemoryAccessType
import chisel3._

import java.lang.reflect.Field
import scala.collection.mutable.{ArrayBuffer, ArrayStack}
class GenModule extends Module


class GenParam{
  var Desc :Map[String,String] = Map()
  //var ParaList:Map[String,Any] = genParamMap()
  def toIR()={
    val list = this.getClass.getDeclaredFields
    var pMap:ArrayBuffer[Map[String,Any]]= ArrayBuffer()
    list.foreach(x=>
      if(!x.getName.contains("bitmap$init$0")) {
        var p_map:Map[String,Any] = Map()
        var desc:String ="Warning: Please add description for "+x.getName
        if(Desc.contains(x.getName)){
          desc =Desc(x.getName)
        }
        p_map +=("Name" -> x.getName)
        p_map +=("Desc"-> desc)
        p_map +=("Value"->this.getClass.getMethod(x.getName).invoke(this))

        pMap  +=(p_map)
      })
    pMap.toSeq
  }

}

class GenBundle extends Bundle{
  var Attr:Map[Data,fldAttr] = Map()
}

case class fldAttr(Desc:String,ResetValue:Long=0L,ExpandArr:Boolean=false)

object GenFieldListFromBundle{
  private def FieldMap(name:String, width:Int, Desc:String)={
    var fld_map:Map[String,Any] = Map()
    fld_map +=("Name"->name)
    fld_map +=("Width"->width)
    fld_map +=("Desc"->Desc)
    fld_map
  }

  def apply(tt:Data,name_d:String="", desc_d:String=""):ArrayBuffer[Map[String,Any]] ={
    var FieldList: ArrayBuffer[Map[String, Any]] = ArrayBuffer()
    tt match {
      case t: GenBundle =>
        val Attr = t.Attr
        val fields = t.elements
        for (fld <- fields) {
          /** Initialize the desc, reset value, expand or not */
          var desc = "Warning: Please add description to the field!!"
          var expand = true
          var reset_value = 0L
          if (Attr.contains(fld._2)) {
            desc = Attr(fld._2).Desc
            expand = Attr(fld._2).ExpandArr
            reset_value = Attr(fld._2).ResetValue
          }
          // if it's a vector
          fld._2 match {
            case datas: Vec[Data] =>
              if(expand) {
                val fl = GenFieldListFromBundle(datas, fld._1, desc)
                fl.map(x => FieldList += FieldMap(fld._1 + "_" + x("Name"), x("Width").toString.toInt, x("Desc").toString))
              }else{
                FieldList += FieldMap(fld._1,fld._2.getWidth,desc)
              }
            case datas: GenBundle =>
              val fl = GenFieldListFromBundle(datas, fld._1, desc)
              fl.map(x => FieldList += x)
            case _ =>
              val a = FieldMap(fld._1, fld._2.getWidth, desc)
              FieldList += a
          }
        }
        FieldList.reverse
      case datas: Vec[Data] =>
        var vec_field_list: ArrayBuffer[Map[String, Any]] = ArrayBuffer()
        val ele = datas.head
        val len = datas.length

        ele match {
          case bo: Bool => {
            (0 until len) .map(i=> vec_field_list+=FieldMap( i.toString, ele.getWidth, desc_d))
            vec_field_list
          }
          case ui: UInt => {
            (0 until len) .map(i=> vec_field_list+=FieldMap( i.toString, ele.getWidth, desc_d))
            println(vec_field_list)
            vec_field_list
          }
          case bundle: GenBundle => {
            val sf = GenFieldListFromBundle(ele.asInstanceOf[GenBundle], name_d, desc_d)
            for (i <- 0 until len) {
              for (f <- sf) {
                val fm = FieldMap(i +"_"+ f("Name").toString, f("Width").toString.toInt, f("Desc").toString)
                vec_field_list += fm
              }
            }
            vec_field_list
          }
          case vd: Vec[Data] => {
            val sf = GenFieldListFromBundle(ele.asInstanceOf[Vec[Data]], name_d, desc_d)
            for (i <- 0 until len) {
              for (f <- sf) {
                val fm = FieldMap(i.toString + "_"+f("Name").toString, f("Width").toString.toInt, f("Desc").toString)
                vec_field_list += fm
              }
            }
            vec_field_list
          }
        }
        case _ =>
          println("Error: Not support data type!")
          FieldList
      }
    }
}

abstract class GenDataStructure{
  var DataStructMap:Map[String,GenBundle] = Map()
  def toIR={
    var targetMap :Map[String,Any] = Map()
    for(i<-DataStructMap.keys){
      targetMap +=(i->GenFieldListFromBundle(DataStructMap(i)))
    }
    targetMap
  }
}

abstract class GenMemory{
  var MemoryArr:ArrayBuffer[Memory] = ArrayBuffer()
  def toIR() = {
    MemoryArr.map(x=>x.toMap)
  }
}

object MemoryAccessType extends Enumeration {
  type MemoryAccessType = Value
  val SP, TP, DP, TCAM  = Value
}

case class Memory(
                 Name:String,
                 dt:Data,
                 depth:Int,
                 AccessType:MemoryAccessType,
                 InstNum:Int,
                 Protect:String="Default",
                 MaskSize:Int = 0,
                 InputPipe:Boolean = false,
                 OutputPipe:Boolean = true,
                 InitValue:String = "0",
                 ErrInsert:Boolean = true,
                 Hazard:Boolean = false,
                 EccTh:Int = 2048,
                 Fatal:Boolean = false
                 ){
  val Width = dt.getWidth
  def toMap={
    var map:Map[String,Any] = Map()
    map +=("Name"->Name)
    map +=("AccessType"->AccessType)
    map +=("Width" -> Width)
    map +=("Depth" -> depth)
    map +=("InstNum"->InstNum)
    map


  }
//  val protectType = GenCheckType(Width,depth,Protect)
}

//abstract class GenMemory{
//  val TCAM = Memory()
//}

abstract class GenIR{
  var IR:Map[String,Any] = Map()
  val MemoryList:GenMemory
  val Param :GenParam
  val DataStruct:GenDataStructure

  def toIR ={
    IR +=("Parameter" -> Param.toIR)
    IR +=("MemoryList" -> MemoryList.toIR)
    IR +=("DataStructure" -> DataStruct.toIR)
    IR
  }


}
