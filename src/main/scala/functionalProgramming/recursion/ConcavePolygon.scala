package functionalProgramming.recursion

import utils.SetInt


/**
  * Created by yujieshui on 2016/7/18.
  */
object ConcavePolygon {

  import scala.math.Pi

  case class Paint(x: Double, y: Double) {
    def *(paint: Paint) = Paint(this.x * paint.x, this.y * paint.y)

    def -(paint: Paint) = Paint(this.x - paint.x, this.y - paint.y)

    def +(paint: Paint) = Paint(this.x + paint.x, this.y + paint.y)

    lazy val angle  = (math.atan2(y, x) + (2 * Pi)) % (2 * Pi)
    lazy val length = math.sqrt(x * x + y * y)

    override def toString: String = {s"""x=$x,y=$y,angle=$angle,length=$length"""}
  }

  type Paints = List[Paint]

  case class Line(start: Paint, end: Paint)

  case class PaintDirection(start: Paint, test: Paint, end: Paint) {
    val isLeft  = (end.x - start.x) * (test.y - start.y) - (end.y - start.y) * (test.x - start.x) > 0
    val isRight = !isLeft
  }

  def centerPaintOf(paints: Paints) = {
    val r = paints reduce (_ + _)
    Paint(r.x / paints.size, r.y / paints.size)
  }

  def paintsToPaintDirection(paints: Paints) = {
    def impl(paints: Paints, rt: Seq[PaintDirection]): Seq[PaintDirection] = paints match {
      case start :: test :: end :: other => impl(paints.tail, PaintDirection(start, test, end) +: rt)
      case other                         => rt
    }
    impl(paints ++ paints.take(2), Nil)
  }

  def solution(paints: Paints): Boolean = {
    val centerPaint = centerPaintOf(paints)
    val new_paints = paints map (_ - centerPaint) sortWith ((l, r) =>
      (l.angle < r.angle) || (l.angle == r.angle && l.length < r.length)
      )
    paintsToPaintDirection(new_paints).exists(_.isLeft)
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val in = 1 to n map (_ => {
      val x :: y :: Nil = readListInt()
      Paint(x, y)
    })
    println(solution(in.toList) match {
      case true  => "YES"
      case false => "NO"
    })
  }

  SetInt(
    """4
      |0 0
      |0 1
      |1 1
      |1 0
    """.stripMargin)
}


//case class Triangle(left: Paint, mid: Paint, right: Paint) {
//  private lazy val a = (left - mid).length
//  private lazy val b = (right - mid).length
//  private lazy val c = (left - right).length
//
//  lazy val isObtuse = {
//    /**/ if (a > b && a > c) a * a > (b * b + c * c)
//    else if (b > a && b > c) b * b > (a * a + c * c)
//    else if (c > a && c > b) c * c > (a * a + b * b)
//    else ???
//  }
//
//  lazy val isRight = {
//    /**/ if (a > b && a > c) a * a == (b * b + c * c)
//    else if (b > a && b > c) b * b == (a * a + c * c)
//    else if (c > a && c > b) c * c == (a * a + b * b)
//    else ???
//  }
//}
//def paintsToTriangle(paints: Paints) = {
//  def impl(paints: Paints, rt: Seq[Triangle]): Seq[Triangle] = {
//  if (paints.size < 3)
//  rt
//  else {
//  val left :: mid :: right :: Nil = paints.take(3)
//  impl(paints.tail, Triangle(left, mid, right) +: rt)
//}
//}
//  val new_paints = paints ++ paints.take(2)
//  impl(new_paints, Nil)
//}
