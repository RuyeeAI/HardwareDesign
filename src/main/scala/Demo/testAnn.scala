package Demo
import chisel3._
import chisel3.experimental.hierarchy.{Definition, Hierarchy, Instance, instantiable}
import firrtl.annotations.NoTargetAnnotation
case object EmptyAnnotation extends NoTargetAnnotation
case class MyChiselAnnotation(m: Hierarchy[RawModule], tag: String) extends experimental.ChiselAnnotation {
  def toFirrtl = {
    println(tag + ": " + m.toTarget)
    EmptyAnnotation
  }
}

@instantiable
class EmptyModule extends Module {
  println("Elaborating EmptyModule!")
}

@instantiable
class TwoEmptyModules extends Module {
  val definition = Definition(new EmptyModule)
  val i0         = Instance(definition)
  val i1         = Instance(definition)
}

class Top extends Module {
  val definition = Definition(new TwoEmptyModules)
  val instance   = Instance(definition)
  aop.Select.allInstancesOf[EmptyModule](instance).foreach { i =>
    experimental.annotate(MyChiselAnnotation(i, "instance"))
  }
  aop.Select.allDefinitionsOf[EmptyModule](instance).foreach { d =>
    experimental.annotate(MyChiselAnnotation(d, "definition"))
  }
}