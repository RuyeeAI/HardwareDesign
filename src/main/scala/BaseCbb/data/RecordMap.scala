// See LICENSE.SiFive for license details.

package BaseCbb.data

import chisel3._
import scala.collection.immutable.ListMap
import chisel3.reflect.DataMirror.internal.chiselTypeClone

final class RecordMap[T <: Data] (eltMap: ListMap[String, T])
    extends Record {

  // chisel 7 移除了内部 API `chisel3.internal.requireIsChiselType`；非法元素会在下面的
  // chiselTypeClone 处报错，这里不再单独做"是否为 chisel 类型"的前置检查。

  // This is needed for Record
  val elements = ListMap[String, T]() ++ eltMap.mapValues(chiselTypeClone(_).asInstanceOf[T])  // mapValues return value is lazy

  def apply(x: Int) = elements.values.toSeq(x)
  def apply(x: String) = elements.get(x)
  def size = elements.size
  def data = elements.values

}

object RecordMap {

  def apply[T <: Data](eltMap: ListMap[String, T]) = new RecordMap(eltMap)

  def apply[T <: Data](elements: (String, T)*): RecordMap[T] = {
    new RecordMap[T](ListMap[String, T](elements:_*))
  }
}
