package functionFractal

import scala.collection.immutable.IndexedSeq

/**
  * Created by yuJieShui on 2016/1/9.
  */
object FractalTree extends App {
  val n     = 2
  val index = (0 to 5 map (math.pow(2, _).toInt)).reverse
  type Tree = Seq[String]
  println(
    fractalTree(n).map(_.mkString("\n")).mkString("\n")
  )

  case class Y(v: Tree, l: Tree, h: Int, __Triangle: Tree, y: Tree)

  def mkY(i: Int): Y = {
    val h = index(i - 1) / 2
    val __Triangle = 1 to (h, 2) map ("_" * _)reverse
    val v: Tree = __Triangle map (e ⇒ "1" + e + "1")
    val l: Tree = 1 to h map (_ ⇒ "1")
    val y: Tree = ( "" +: __Triangle.tail.reverse, v,"" +:  __Triangle.tail.reverse).zipped.map {
      case (l: String, v: String, r) ⇒ l + v + r
    } ++ l.map {
      case i ⇒ __Triangle.head + i + __Triangle.head
    }

    Y(v, l, h, __Triangle, y)
  }

  def fractalTree(i: Int): IndexedSeq[Tree] = {
    val a: IndexedSeq[Y] = (1 to n map (e ⇒ mkY(e))).reverse
    a.map(_.y)
  }
}
