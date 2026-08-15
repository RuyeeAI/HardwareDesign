// See LICENSE.SiFive for license details.

package BaseCbb.utils.annotation
import chisel3._
import chisel3.experimental.{annotate, ChiselAnnotation}
import chisel3.RawModule
import firrtl.annotations._

import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{pretty, render}

/** Record a sram. */
case class SRAMAnnotation(target: Named,
  address_width: Int,
  name: String,
  data_width: Int,
  depth: BigInt,
  description: String,
  write_mask_granularity: Int) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

/** Record a set of interrupts. */
case class InterruptsPortAnnotation(target: Named, name: String, interruptIndexes: Seq[Int]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

/** Record a case class that was used to parameterize this target. */
case class GlobalConstantsAnnotation(target: Named, xLen: Int) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

case class GlobalConstantsChiselAnnotation[T <: Product](target: InstanceId, xLen: Int) extends ChiselAnnotation {
  def toFirrtl = GlobalConstantsAnnotation(target.toNamed, xLen)
}

/** Record a case class that was used to parameterize this target. */
case class ParamsAnnotation(target: Named, paramsClassName: String, params: Map[String,Any]) extends SingleTargetAnnotation[Named] {
  def duplicate(n: Named) = this.copy(n)
}

case class ParamsChiselAnnotation[T <: Product](target: InstanceId, params: T) extends ChiselAnnotation {
  private val paramMap = params.getClass.getDeclaredFields.map(_.getName).zip(params.productIterator).toMap
  def toFirrtl = ParamsAnnotation(target.toNamed, params.getClass.getName, paramMap)
}
object Annotated {

  def srams(
    component: InstanceId,
    name: String,
    address_width: Int,
    data_width: Int,
    depth: BigInt,
    description: String,
    write_mask_granularity: Int): Unit = {
    annotate(new ChiselAnnotation {def toFirrtl: Annotation = SRAMAnnotation(
      component.toNamed,
      address_width = address_width,
      name = name,
      data_width = data_width,
      depth = depth,
      description = description,
      write_mask_granularity = write_mask_granularity
    )})}

  def interrupts(component: InstanceId, name: String, interrupts: Seq[Int]): Unit = {
    annotate(new ChiselAnnotation {def toFirrtl: Annotation = InterruptsPortAnnotation(
      component.toNamed,
      name,
      interrupts
    )})}
}
