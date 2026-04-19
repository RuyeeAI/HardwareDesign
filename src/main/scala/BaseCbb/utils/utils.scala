package BaseCbb.utils
//
import chisel3._
import chisel3.util._
import chisel3.experimental._


object GenProcessBuilder{
  def apply(cmd:String):Process={
    val cmd_str = cmd.split(" ").map(x=> "\"" + x+"\"").mkString(" ")
    println(s"System Command Excute: $cmd")
    val delay_process = new ProcessBuilder(cmd_str)
    delay_process.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    delay_process.redirectError(ProcessBuilder.Redirect.INHERIT)
    delay_process.start()
  }
}

object Seq2Vec{
  def apply[T<:Data](s:Seq[T]):Vec[T]={
    val v = Wire(Vec(s.length,s.head.cloneType))
//    println("Seq2Vec:",s.length)
    (0 until s.length).map(i=>v(i):=s(i))
    v
  }
}

object SubVec{
  def apply[T<:Data](v:Vec[T],st:Int,size:Int)={
//    println("SubVec "+v.length,st,size)
    Seq2Vec((st until st+size).map(i=>v(i)))
  }
}

object Convert2dArray{
  def apply[T<:Data](v:Vec[Vec[T]]):Vec[Vec[T]] = {
    val cv = Wire(Vec(v.head.length,Vec(v.length,v.head.head.cloneType)))
    for(i<-0 until v.length){
      for(j<-0 until v.head.length){
        cv(j)(i) := v(i)(j)
      }
    }
    cv
  }
}






//import level1_cbb.utils.fpp_utils.{B, BitCount}
//
//import scala.collection.immutable.ListMap
//
//sealed trait indexReduce extends (((UInt,UInt),(UInt,UInt))=>(UInt,UInt))
//
//object all_ones{
//  def apply(a:Int): UInt = Cat(VecInit(Seq.fill(a)(1.U)))
//}
//
//sealed trait etherBoolean extends(UInt=>Bool)
//
//object is_true extends etherBoolean{
//  def apply(a:UInt): Bool = a===1.U
//}
//
//object is_false extends etherBoolean{
//  def apply(a:UInt): Bool = a===0.U
//}
//
//sealed trait etherReduce extends((UInt,UInt)=>UInt)
//
//object etherOR extends etherReduce{
//  def apply(a:UInt,b:UInt): Bool =is_false(a|b)
//}
//
////object etherAND extends etherReduce{
////  def apply(a:UInt,b:UInt): Bool =(a.andR() && b.andR())
////}
//
//object ReduceMin extends etherReduce{
//  def apply(a:UInt,b:UInt)=Mux(a>b,b,a)
//}
//
//object ReduceMax extends etherReduce{
//  def apply(a:UInt,b:UInt)=Mux(a>b,a,b)
//}
//
////object padto{
////  def apply[T<:Data,S<:Data](t:T,s:S,left:Boolean=false):T={
////    val sw = s.getWidth
////    val tw = t.getWidth
////
////    val v = WireInit(0.U(tw.W))
////    if(sw<tw) v:= {if(!left) Cat(0.U((tw-sw).W),s.asUInt()) else Cat(s.asUInt(),0.U((tw-sw).W))}
////    else      v:= {if(!left) s.asUInt()(tw-1,0) else s.asUInt()(sw-1,sw-tw)}
////    t match{
//////      case bundle:GenBundle => {
//////        val w = Wire(bundle.cloneType)
//////        w:=* v
//////        println(s"${w.getWidth}")
//////        w.asInstanceOf[T]
//////      }
////      case _ => v.asTypeOf(t)
////    }
////  }
////}
////
////object padBits{
////  def apply[T<:Data](w:Int,s:T):UInt={
////    val sw = s.getWidth
////    val tw = w
////    if(sw == tw) s.asUInt()
////    else if(sw>tw) s.asUInt()(tw-1,0)
////    else          Cat(0.U((tw-sw).W),s.asUInt())
////  }
////}
////
////object maskbetween{
////  def apply(lhs:Int,rhs:Int,w:Int):UInt={
////    val init = WireInit(VecInit(Seq.fill(w)(0.U(1.W))))
////    for(i<-rhs until lhs+1){
////      init(i) := 0.U(1.W)
////    }
////    init.asUInt()
////  }
////}
////
//object ShiftArrayEn{
//  def apply[T<:Data] (in:Seq[T],n:Int,init:T,en:Seq[Bool],name:Option[String]= None): Seq[T] ={
//    require(in.length==en.length)
//    in.zipWithIndex.map{case(v,i)=>{ShiftRegEn(v,n,init,en(i),name)}}
//  }
//}
//
object ShiftRegEn {
  def apply[T<:Data](in: T, n: Int, init: T, en: chisel3.Bool, name: Option[String]): T = {
    val (o,u)=(0 until n).reverse.foldRight(in,en){
      case(i,next)=> {
        val r = RegEnable(next._1, init, next._2).suggestName(s"${name}_$i")
        val e = RegNext(next._2).suggestName(s"en_$i")
        (r, e)
      }
    }
    o
  }

  def apply[T<:Data](in: T, n: Int, en: chisel3.Bool, name: String): T = {
    val (o,u)=(0 until n).reverse.foldRight(in,en){
      case(i,next)=> {
        val r = RegEnable(next._1, next._2).suggestName(s"${name}_$i")
        val e = RegNext(next._2).suggestName(s"en_$i")
        (r, e)
      }
    }
    o
  }
}
//
////object OH1ToOH{
////  def apply(x:UInt):UInt = x & ((~x).asUInt()+1.U)
////}
//
//
////object OH1ToUInt{
////  def apply(x:UInt):UInt = OHToUInt(OH1ToOH(x))
////}
//
//object UIntToOH1{
//  def apply(x:UInt,Width:Int):UInt = (all_ones(Width)<<x)(Width-1,0)
//  def apply(x:UInt):UInt = UIntToOH1(x,(1<<x.getWidth)-1)
//}
//
//object trailingZeros{
//  def apply(x:Int):Option[Int] = if(x>0) Some(log2Ceil(x & -x)) else None
//}
//
////object Not{
////  def apply(x:UInt):UInt = (~x).asUInt()
////}
////
//object _0b{
//  def apply(row:String):Int={
//    row.reverse.split("").zipWithIndex.map(x=>(x._1.toInt,x._2)).filter(_._1 ==1).map(x=>Math.pow(2,x._2).toInt).sum
//  }
//}
//
//object onehotEnum{
//  def apply(n:Int): List[UInt] ={
//    (0 until n).map(i=>(1<<i).U((1 max n).W)).toList
//  }
//}
//
//object findMatchInPort{
//  def apply[T<:Data](elts:ListMap[String,T],regex:String): Option[(String, T)] = {
//    def found = elts.map { case (name, elt) => (name matches regex) -> name }.filter(_._1).values
//    if(found.toList.nonEmpty){
//      Some((found.head->elts(found.head)))
//    }else{
//      None
//    }
//  }
//}
//
//
//package object fpp_utils{
//
//
//  implicit class CloneBundleLiteralConstructor[T<:Record](x:T){
//    def getFirstLevelFields(data:Record)=data.elements.map{case(fieldName,fieldData)=> fieldData->fieldName}
//    def makeLit(elems:(T=>(Data,Data))*)={
//      val clone = x.cloneType
//      val cloneFields = getFirstLevelFields(clone).toMap
//      elems.map{fn=>fn(clone)}.flatMap{case(field,value)=>
//        val fieldName = cloneFields.getOrElse(field, throw new Exception(s"field $field(with value $value) is not a field,"+s"ensure the field is specified as a function returning a field on and object of class ${this.getClass},"+s" eg '_.a' to select hypothetical bundle field 'a'"))
//        Seq(fieldName -> value)
//      }
//    }
//    def copy(elts:(T=>(Data,Data))*):T={
//      val rhs = WireInit(x)
//      makeLit(elts:_*) foreach {case (field,elt)=> rhs.elements(field):= elt}
//      rhs
//    }
//  }
//
//  case class BitCount(value:Int){
//    def +(right:BitCount): BitCount = BitCount(this.value+right.value)
//    def -(right:BitCount): BitCount = BitCount(this.value-right.value)
//    def *(right:BitCount): BitCount = BitCount(this.value*right.value)
//    def /(right:BitCount): BitCount = BitCount(this.value/right.value)
//    def %(right:BitCount): BitCount = BitCount(this.value%right.value)
//  }
//  object U extends UIntFactory{
//    def apply(in:Bool):UInt = in.asUInt()
//    def apply(in:Bits):UInt = in.asUInt()
//    def apply(in:SInt):UInt = in.asUInt()
//    def apply(in:Int):UInt = in.U
//    def apply(in:Long):UInt = in.U
//
//    def apply(in:Bool,width:Int) :UInt = in.asTypeOf(UInt(width.W))
//    def apply(in:Bits,width:Int) :UInt = in.asTypeOf(UInt(width.W))
//    def apply(in:SInt,width:Int) :UInt = in.asTypeOf(UInt(width.W))
//
//  }
//
//  object B extends UIntFactory{
//    def apply(in:Boolean):Bool = in.B
//    def apply(in:BitCount):Bits  = Bits(in.value.W)
//    def apply(section:Range,init:UInt)= WireInit(VecInit(section.map(i=>init(i))))
//    def apply(section:Range)= WireInit(VecInit(section.map(i=>false.B)))
//  }
//
////  implicit class UnzippableOption[S,T](val x:Option[(S,T)]){
////    def unzip: (Option[S], Option[T]) = (x.map(_._1),x.map(_._2))
////  }
//
//  implicit class SeqToAugmentedSeq[T<:Data](val x:Seq[T]){
//    def apply(idx:UInt):T={
//      if(x.size <=1){
//        x.head
//      }else if(!isPow2(x.size)){
//        (x++x.takeRight(x.size & ~x.size)).toSeq(idx)
//      }else{
//        val truncIdx = if(idx.isWidthKnown && idx.getWidth<=log2Ceil(x.size)) idx else (idx|0.U(log2Ceil(x.size).W)) (log2Ceil(x.size)-1,0)
//        (x.head/:x.zipWithIndex.tail){case(prev,(cur,i))=>Mux(truncIdx===i.U,cur,prev)}
//      }
//    }
//
//    def asUInt():UInt = Cat(x.map(_.asUInt()).reverse)
//    def rotate(n:Int):Seq[T] = x.drop(n) ++ x.take(n)
//    def rotate(n:UInt):Seq[T] = {
//      require(isPow2(x.size))
//      val amt = n.padTo(log2Ceil(x.size))
//      (x/:(0 until log2Ceil(x.size)))((r,i)=>(r.rotate(1<<i) zip r).map{case(s,a)=>Mux(amt(i),s,a)})
//    }
//    def rotateRight(n:Int) :Seq[T] = x.takeRight(n) ++ x.dropRight(n)
//    def rotateRight(n:UInt):Seq[T] = {
//      require(isPow2(x.size))
//      val amt = n.padTo(log2Ceil(x.size))
//      (x/:(0 until log2Ceil(x.size)))((r,i)=>(r.rotateRight(1<<i) zip r).map{case(s,a)=>Mux(amt(i),s,a)})
//    }
////    def priorityEncoderOH:Seq[Bool] = {
////      x.map(v=>require(v.getWidth==1))
////      chisel3.util.PriorityEncoderOH(x.map(_.asUInt().asBool()))
////    }
//    def reduceTree(redOP:(T,T)=>T):T={
//      VecInit(x).reduceTree(redOP)
//    }
//  }
//
//  implicit class SeqBoolBitwiseOps(val x:Seq[Bool]){
//    def &(y:Seq[Bool]):Seq[Bool] = (x zip y).map(x=>x._1 && x._2)
//    def |(y:Seq[Bool]):Seq[Bool] = (x zip y).map(x=>x._1 || x._2)
//    def ^(y:Seq[Bool]):Seq[Bool] = (x zip y).map(x=>x._1 ^ x._2)
//    def <<(n:Int):Seq[Bool] = Seq.fill(n)(false.B)++x
//    def >>(n:Int):Seq[Bool] = x.drop(n)
//    def unary_~():Seq[Bool] = x.map(!_)
//    def andR:Bool = if(x.isEmpty) false.B else x.reduce(_&&_)
//    def orR:Bool  = if(x.isEmpty) false.B else x.reduce(_||_)
//    def xorR:Bool = if(x.isEmpty) false.B else x.reduce(_^_)
////    private def padZip(y:Seq[Bool],z:Seq[Bool]):Seq[(Bool,Bool)] = y.PadTo(z.size,false.B) zip z.padTo(y.size,false.B)
//    def priorityEncoderOH:Seq[Bool] = chisel3.util.PriorityEncoderOH(x)
//  }
//
////  implicit class BooleanToAugmentedBoolean(val x:Boolean){
////    def toInt:Int = if(x) 1 else 0
////    def option[T](z:=>T):Option[T] = if(x) Some(z) else None
////  }
//
//  implicit class IntBuilder(val x:Int){
//    def downto(start:Int):Range.Inclusive = Range.inclusive(x,start,-1)
//    def log2:Int = log2Ceil(x)
//    def bits = BitCount(x)
//    def bit  = BitCount(x)
//    def Byte = BigInt(x)
//    def KiB  = BigInt(x)<<10
//    def MiB  = BigInt(x)<<20
//    def GiB  = BigInt(x)<<30
//    def TiB  = BigInt(x)<<40
//    def PiB  = BigInt(x)<<50
//    def EiB  = BigInt(x)<<60
//    def ZiB  = BigInt(x)<<70
//    def ceilTo(y:Int) = if(x%y !=0) y*(x/y+1) else x
//  }
//
////  implicit class DataToAugmentedData[T<:Data](val x:T){
////    def holdUnless(enable:Bool):T = Mux(enable,x,RegEnable(x,enable))
////    def =!= (rhs:Int):UInt  = (x.asUInt()=/=rhs.U).asUInt()
////    def =!= (rhs:UInt):UInt = (x.asUInt()=/=rhs).asUInt()
////    def === (rhs:Int):UInt  = (x.asUInt()===rhs.U).asUInt()
////    def <<<(shift_amt:UInt):T = (x.asUInt()<<shift_amt).asTypeOf(x)
////    def >>>(shift_amt:UInt):T = (x.asUInt()>>shift_amt).asTypeOf(x)
////    def <<<(shift_amt:Int):T = (x.asUInt()<<shift_amt.U).asTypeOf(x)
////    def >>>(shift_amt:Int):T = (x.asUInt()>>shift_amt.U).asTypeOf(x)
////    def invert:T = (~x.asUInt()).asTypeOf(x)
////    def reg(n:Int):T = if(n==0) x else ShiftRegEn(x,n,0.U.asTypeOf(x),true.B,Option[String])
////    def reg:T = x.reg(1)
////    def reset:Unit = x:=0.U.asTypeOf(chiselTypeOf(x))
////    def reset(rhs:Int) = x:=rhs.U.asTypeOf(chiselTypeOf(x))
////    def reset[S<:Data](rhs:S) = x:=rhs.asUInt().asTypeOf(chiselTypeOf(x))
////    def setAll() = x:=Seq.fill(x.getWidth){1.U}.asUInt().asTypeOf(x.cloneType)
////    def setAll(w:Int) = x:=Seq.fill(w){1.U}.asUInt().asTypeOf(x.cloneType)
////    def clear() = x.reset
////    def set() = true.B
////    def get(section:Range) = {
////      val n = WireInit(x.asUInt())
////      section.map(i=>n(i)).reverse.asUInt()
////    }
////  }
//
//  implicit class UIntToAugmentedUInt(val x:UInt){
//    def isOneOf(s:Seq[UInt]):Bool = s.map(x===_).orR
//    def isOneOf(u1:UInt,u2:UInt*):Bool = isOneOf(u1 +: u2.toSeq)
//    def &(rhs:Int):UInt    = x & rhs.U
//    def &(rhs:BigInt):UInt = x & rhs.U
//    def &(rhs:Long):UInt   = x & rhs.U
//
//    def <(rhs:Int)   :Bool = x < rhs.U
//    def <(rhs:BigInt):Bool = x < rhs.U
//    def <(rhs:Long)  :Bool = x < rhs.U
//
//    def >=(rhs:Int)   :Bool = x >= rhs.U
//    def >=(rhs:BigInt):Bool = x >= rhs.U
//    def >=(rhs:Long)  :Bool = x >= rhs.U
//
//    def ===(rhs:Int)   :Bool = x === rhs.U
//    def ===(rhs:BigInt):Bool = x === rhs.U
//    def ===(rhs:Long)  :Bool = x === rhs.U
//
//    def -(rhs:Int):UInt    = x - rhs.U
//    def -(rhs:BigInt):UInt = x - rhs.U
//    def -(rhs:Long):UInt   = x - rhs.U
//
//    def setTo(n:Int) :UInt = {
//      require((x.getWidth <=n))
//      if(x.getWidth==n) x else Cat(Fill(n-x.getWidth,x(x.getWidth-1)),x)
//    }
//
//    def padTo(n:Int):UInt ={
//      require((x.getWidth <=n))
//      if(x.getWidth==n) x else Cat(0.U((n-x.getWidth).W),x)
//    }
//
//    def extract(hi:Int,lo:Int):UInt = {
//      require(hi>=lo-1)
//      if(hi==lo-1) 0.U else x(hi,lo)
//    }
//
//    def andNot(y:UInt) :UInt = x & (~(y|(x&0.U))).asUInt()
//
//    def rotateRight(n:Int):UInt = if(n==0) x else Cat(x(n-1,0),x>>n)
//    def rotateRight(n:UInt):UInt = {
//      val amt = n.padTo(log2Ceil(x.getWidth))
//      (x/:(0 until log2Ceil(x.getWidth)))((r,i)=>Mux(amt(i),r.rotateRight(1<<i),r))
//    }
//    def rotateLeft(n:Int):UInt = if(n==0) x else Cat(x(x.getWidth-1-n,0),x(x.getWidth-1,x.getWidth-n))
//    def rotateLeft(n:UInt):UInt = {
//      val amt = n.padTo(log2Ceil(x.getWidth))
//      (x/:(0 until log2Ceil(x.getWidth)))((r,i)=>Mux(amt(i),r.rotateLeft(1<<i),r))
//    }
//  }
//
//
//  implicit class tuple2Assign[TT0<:Data,TT1<:Data](in:Tuple2[TT0,TT1]){
//    def :=[TA0<:Data,TA1<:Data](tuple:Tuple2[TA0,TA1]): Unit ={
//      in._1 := tuple._1.asTypeOf(in._1)
//      in._2 := tuple._2.asTypeOf(in._2)
//    }
//    def :=[A<:Data] (tuple:List[A])={
//      in._1 := tuple(0).asTypeOf(in._1)
//      in._2 := tuple(1).asTypeOf(in._2)
//    }
////    def reg(n:List[Int]) = {in.asInstanceOf[List[Data]].zip(n).map(case(d,i)=> d reg i)}
//
//  }
//
////  implicit class CommonBundleNameConnectTo[T<:GenBundle](lhs:T){
////    def *:=* [S<:GenBundle](rhs:S,ignore_printer:Boolean=true)(implicit line:sourcecode.Line,file:sourcecode.File)={
////      val stack = s"${file.value}:${line.value}"
////      lhs := 0.U.asTypeOf(lhs.cloneType)
////      lhs.uncond_move_to_left(rhs)(stack)(ignore_printer)
////    }
////
////    def :=* [S<:Data](rhs:S)={
////      lhs := 0.U.asTypeOf(lhs.cloneType)
////      lhs.uncond_move_nwvec_left(rhs)
////    }
////  }
//}
//
//
//object Vec2Seq{
//  def apply(in:Vec[Data]):Seq[Data]={
//    val in_s = in.map(x=>x)
//    in_s
//  }
//}
//
//
//
//object PriMux{
//  def apply(bitmap:Seq[Bool],in:Seq[Data]):(Bool,Data)={
//    if(bitmap.length==1){
//      (bitmap.head,in.head)
//    }else if(bitmap.length==2){
//      (bitmap.reduce(_|_),Mux(bitmap.head,in.head,in.last))
//    }else {
//      val bitmap_h1  = bitmap.take(math.ceil(bitmap.length / 2).toInt)
//      val in_h1      = in.take(math.ceil(bitmap.length / 2).toInt)
//      val bitmap_h2  = bitmap.drop(math.ceil(bitmap.length / 2).toInt)
//      val in_h2      = in.drop(math.ceil(bitmap.length / 2).toInt)
//      val (b_1, i_1) = PriMux(bitmap_h1, in_h1)
//      val (b_2, i_2) = PriMux(bitmap_h2, in_h2)
//      (b_1||b_2,Mux(b_1,i_1,i_2))
//    }
//  }
//}
//
//object test extends App{
//  println(2)
//  println(-2)
//  println(2 & (-2))
//  println(trailingZeros(2))
//  println(trailingZeros(3))
//  println(trailingZeros(4))
//  println(trailingZeros(5)) // 10
//  println(trailingZeros(128)) // 10
//  println(trailingZeros(129)) // 10
//  println(B(true).litValue())
//  val a = BitCount(2)
//  val c = BitCount(2)
//  val b = a+c
//  println(b)
//
//  val xx = VecInit(Seq.fill(5)(UInt(3.W)))
//
//
//}
