package Maze

import BaseCbb.data.{GenBundle, GenModule}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import scala.collection.mutable.ArrayBuffer

class IntfC(Num:Int) extends GenBundle{
  val idt = Flipped(Vec(Num,DecoupledIO(new PktDataBundle())))
  val odt = Vec(Num,DecoupledIO(new PktDataBundle()))
}

class TOPO (Connection:ArrayBuffer[ArrayBuffer[ArrayBuffer[(Int,Int)]]])extends GenModule{
  val Num = Connection.head.head.length
  val io = IO(new Bundle {
    val d = Vec(8,Vec(8,new IntfC(Num)))
  })
  for(i<- Connection.indices){
    for(j<- Connection.head.indices){
      for(k<-0 until Num){
        val conn = Connection(i)(j)(k)
       io.d(i)(j).odt(k) <> io.d(conn._1)(conn._2).idt(k)
      }
    }
  }
}
class Maze(Connection:ArrayBuffer[ArrayBuffer[ArrayBuffer[(Int,Int)]]] )extends GenModule{
  val w = Connection.length
  val h = Connection.head.length
  val io = IO(new Bundle{
    val ipkt = Flipped(Vec(w,Vec(h,DecoupledIO(new PktDataBundle()))))
    val opkt = Vec(w,(Vec(h,DecoupledIO(new PktDataBundle()))))
  })

  val U_TOPO = Module(new TOPO(Connection))
  for(i<-0 until(w)){
    for(j<-0 until(h)){
      val U_NODE = Module(new Node((i,j),Connection))
      U_NODE.io.i_pkt <> io.ipkt(i)(j)
      U_NODE.io.o_pkt <> io.opkt(i)(j)
      U_NODE.io.intfc <> U_TOPO.io.d(i)(j)
    }
  }
}



class TopoParamTorus {
  def GenParam(w: Int, h: Int): ArrayBuffer[ArrayBuffer[ArrayBuffer[(Int, Int)]]] = {
    var row: ArrayBuffer[ArrayBuffer[ArrayBuffer[(Int, Int)]]] = ArrayBuffer()
    for (i <- 0 until w) {
      var col: ArrayBuffer[ArrayBuffer[(Int, Int)]] = ArrayBuffer()
      for (j <- 0 until h) {
        var a: ArrayBuffer[(Int, Int)] = ArrayBuffer()
        var ip1 = i + 1
        var im1 = i - 1
        var jp1 = j + 1
        var jm1 = j - 1
        if (ip1 == w) {
          ip1 = 0
        }
        if (jp1 == h) {
          jp1 = 0
        }
        if (im1 == -1) {
          im1 = w - 1
        }
        if (jm1 == -1) {
          jm1 = h - 1
        }
        a.append((i, jp1))
        a.append((i, jm1))
        a.append((im1, j))
        a.append((ip1, j))
        col.append(a)
      }
      row.append(col)
    }
    print(row)
    row
  }

  val torusParam = ArrayBuffer(
    ArrayBuffer(
      ArrayBuffer((0, 1), (0, 7), (7, 0), (1, 0)),
      ArrayBuffer((0, 2), (0, 0), (7, 1), (1, 1)),
      ArrayBuffer((0, 3), (0, 1), (7, 2), (1, 2)),
      ArrayBuffer((0, 4), (0, 2), (7, 3), (1, 3)),
      ArrayBuffer((0, 5), (0, 3), (7, 4), (1, 4)),
      ArrayBuffer((0, 6), (0, 4), (7, 5), (1, 5)),
      ArrayBuffer((0, 7), (0, 5), (7, 6), (1, 6)),
      ArrayBuffer((0, 0), (0, 6), (7, 7), (1, 7))),
    ArrayBuffer(
      ArrayBuffer((1, 1), (1, 7), (0, 0), (2, 0)), ArrayBuffer((1, 2), (1, 0), (0, 1), (2, 1)), ArrayBuffer((1, 3), (1, 1), (0, 2), (2, 2)), ArrayBuffer((1, 4), (1, 2), (0, 3), (2, 3)), ArrayBuffer((1, 5), (1, 3), (0, 4), (2, 4)), ArrayBuffer((1, 6), (1, 4), (0, 5), (2, 5)), ArrayBuffer((1, 7), (1, 5), (0, 6), (2, 6)), ArrayBuffer((1, 0), (1, 6), (0, 7), (2, 7))), ArrayBuffer(ArrayBuffer((2, 1), (2, 7), (1, 0), (3, 0)), ArrayBuffer((2, 2), (2, 0), (1, 1), (3, 1)), ArrayBuffer((2, 3), (2, 1), (1, 2), (3, 2)), ArrayBuffer((2, 4), (2, 2), (1, 3), (3, 3)), ArrayBuffer((2, 5), (2, 3), (1, 4), (3, 4)), ArrayBuffer((2, 6), (2, 4), (1, 5), (3, 5)), ArrayBuffer((2, 7), (2, 5), (1, 6), (3, 6)), ArrayBuffer((2, 0), (2, 6), (1, 7), (3, 7))), ArrayBuffer(ArrayBuffer((3, 1), (3, 7), (2, 0), (4, 0)), ArrayBuffer((3, 2), (3, 0), (2, 1), (4, 1)), ArrayBuffer((3, 3), (3, 1), (2, 2), (4, 2)), ArrayBuffer((3, 4), (3, 2), (2, 3), (4, 3)), ArrayBuffer((3, 5), (3, 3), (2, 4), (4, 4)), ArrayBuffer((3, 6), (3, 4), (2, 5), (4, 5)), ArrayBuffer((3, 7), (3, 5), (2, 6), (4, 6)), ArrayBuffer((3, 0), (3, 6), (2, 7), (4, 7))), ArrayBuffer(ArrayBuffer((4, 1), (4, 7), (3, 0), (5, 0)), ArrayBuffer((4, 2), (4, 0), (3, 1), (5, 1)), ArrayBuffer((4, 3), (4, 1), (3, 2), (5, 2)), ArrayBuffer((4, 4), (4, 2), (3, 3), (5, 3)), ArrayBuffer((4, 5), (4, 3), (3, 4), (5, 4)), ArrayBuffer((4, 6), (4, 4), (3, 5), (5, 5)), ArrayBuffer((4, 7), (4, 5), (3, 6), (5, 6)), ArrayBuffer((4, 0), (4, 6), (3, 7), (5, 7))), ArrayBuffer(ArrayBuffer((5, 1), (5, 7), (4, 0), (6, 0)), ArrayBuffer((5, 2), (5, 0), (4, 1), (6, 1)), ArrayBuffer((5, 3), (5, 1), (4, 2), (6, 2)), ArrayBuffer((5, 4), (5, 2), (4, 3), (6, 3)), ArrayBuffer((5, 5), (5, 3), (4, 4), (6, 4)), ArrayBuffer((5, 6), (5, 4), (4, 5), (6, 5)), ArrayBuffer((5, 7), (5, 5), (4, 6), (6, 6)), ArrayBuffer((5, 0), (5, 6), (4, 7), (6, 7))), ArrayBuffer(ArrayBuffer((6, 1), (6, 7), (5, 0), (7, 0)), ArrayBuffer((6, 2), (6, 0), (5, 1), (7, 1)), ArrayBuffer((6, 3), (6, 1), (5, 2), (7, 2)), ArrayBuffer((6, 4), (6, 2), (5, 3), (7, 3)), ArrayBuffer((6, 5), (6, 3), (5, 4), (7, 4)), ArrayBuffer((6, 6), (6, 4), (5, 5), (7, 5)), ArrayBuffer((6, 7), (6, 5), (5, 6), (7, 6)), ArrayBuffer((6, 0), (6, 6), (5, 7), (7, 7))), ArrayBuffer(ArrayBuffer((7, 1), (7, 7), (6, 0), (0, 0)), ArrayBuffer((7, 2), (7, 0), (6, 1), (0, 1)), ArrayBuffer((7, 3), (7, 1), (6, 2), (0, 2)), ArrayBuffer((7, 4), (7, 2), (6, 3), (0, 3)), ArrayBuffer((7, 5), (7, 3), (6, 4), (0, 4)), ArrayBuffer((7, 6), (7, 4), (6, 5), (0, 5)), ArrayBuffer((7, 7), (7, 5), (6, 6), (0, 6)), ArrayBuffer((7, 0), (7, 6), (6, 7), (0, 7))))
}
object MazeGen extends App{
  //chisel3.Driver.execute(Array[String](xs="--target-dir","Verilog"),()=>new AdderWrapper())
  val par = new TopoParamTorus
  val p = par.GenParam(8,8)

  // 产物统一写 generated/（已 gitignore，可随时重新生成）
  ChiselStage.emitSystemVerilogFile(
    new Maze(p),
    args = Array("--target-dir", "generated"),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
  )}

