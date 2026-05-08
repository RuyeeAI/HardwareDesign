package BaseCbb
import chisel3._
import MemoryAccessType.{MemoryAccessType, SP}
import MemoryInitType.{AllZero, MemoryInitType}
import MemoryProtectType.{MemoryProtectType,ECC,Parity}
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

object MemoryProtectType extends Enumeration{
  type MemoryProtectType = Value
  val ECC,Parity,ProtNone = Value
}
object MemoryInitType extends Enumeration{
  type MemoryInitType = Value
  val AllZero,AllOne,Incr= Value
}


  case class Memory(
                     name:String,
                     dataType:Data,
                     depth:Int,
                     memoryType:MemoryAccessType = SP,
                     instNum:Int = 1,
                     protect:MemoryProtectType = ECC,

                     flopIn:Boolean=false,
                     flopOut:Boolean=true,
                     CheckIn:Boolean=false,
                     CheckOut:Boolean=true,

                     initValue:MemoryInitType= AllZero,

                     Hazard:Boolean = false,
                     protectWidthTh:Int = 320,
                     bypassOnConflict:Boolean = false,
                     Fatal:Boolean = false,
                     RsAccess:Boolean = false,
                     RsMemoryDisLat:Int = 32
                   ) {



    private def log2Ceil(x: Int): Int = if (x <= 1) 0 else (math.ceil(math.log(x.toDouble) / math.log(2))).toInt

    def dataWidth:Int = {
      val eccSegNum = math.ceil(dataType.getWidth.toDouble / protectWidthTh).toInt
      if(protect==ECC) {
        val eccSegWidth = math.ceil(dataType.getWidth / eccSegNum).toInt
        val lastEccSegWidth = dataType.getWidth - (eccSegNum - 1) * eccSegWidth
        val eccTotalWidth = (eccWidth(eccSegWidth) + 1) * (eccSegNum - 1) + (eccWidth(lastEccSegWidth) + 1)
        eccTotalWidth + dataType.getWidth
      }else if(protect == Parity){
        dataType.getWidth + eccSegNum
      }else{
        dataType.getWidth
      }
    }

    def lastCheckSegWidth = {
      val eccSegNum = math.ceil(dataType.getWidth.toDouble / protectWidthTh).toInt
      if(protect==ECC | protect==Parity) {
        val eccSegWidth = math.ceil(dataType.getWidth / eccSegNum).toInt
        dataType.getWidth - (eccSegNum - 1) * eccSegWidth
      }else{
        dataType.getWidth
      }
    }

    def latency :Int = {
      var lat = 1
      if(flopIn){
        lat = lat+1
      }
      if(flopOut){
        lat = lat+1
      }
      lat
    }

    def eccWidth(n:Int):Int={
      val k = log2Ceil(n)
      if(math.pow(2,k)>=(n+k+1)){
        k
      }else{
        k+1
      }
    }

    def addrWidth:Int = log2Ceil(depth)

    def toMap = {
      var map: Map[String, Any] = Map()
      map += ("Name" -> Name)
      map += ("AccessType" -> memoryType)
      map += ("Width" -> dataWidth)
      map += ("Depth" -> depth)
      map += ("InstNum" -> instNum )
      map
  }
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
