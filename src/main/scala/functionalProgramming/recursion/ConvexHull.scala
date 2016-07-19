package functionalProgramming.recursion

import utils.SetInt

/**
  * Created by yujieshui on 2016/7/19.
  */
object ConvexHull {

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

  def isLeft(start: Paint, test: Paint, end: Paint) =
    (end.x - start.x) * (test.y - start.y) - (end.y - start.y) * (test.x - start.x) >= 0

  def centerPaintOf(paints: Paints) = {
    val r = paints reduce (_ + _)
    Paint(r.x / paints.size, r.y / paints.size)
  }

  def filterInlinePaint(paints: Paints): Paints = {
    def impl(paints: Paints, rt: Paints,dropPoints:Paints = Nil): Paints = paints match {
      case start :: test :: end :: other =>
        if (isLeft(start, test, end) && !rt.contains(test) )
          impl(start :: end :: other, rt, test +: dropPoints)
        else
          impl(paints.tail, test +: rt,dropPoints)
      case other                         => rt.filterNot(dropPoints.contains)
    }
    impl( paints ++ paints.take(2), List())
  }

  def solution(paints: Paints): Paints = {
    val centerPaint = centerPaintOf(paints)
    val new_paints = paints map (_ - centerPaint) sortBy (e => e.angle -> e.length)
    val result = filterInlinePaint(new_paints).map( _ + centerPaint)
    if (paints.size == result.size) result else solution(result)
  }

  def perimeter(paints: Seq[Paint]) = {
    paints.foldLeft((0.0, paints.last)) {
      case ((result, head), tail) =>
        val new_result = result +
          math.sqrt((tail.y - head.y) * (tail.y - head.y) +
            (tail.x - head.x) * (tail.x - head.x))
        (new_result,tail)
    }._1
  }
  def main(args: Array[String]) {
    def readListInt(): List[Int] = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val in = 1 to n map (_ => readListInt()) map {
      case x :: y ::Nil => Paint(x,y)
    }
    println(perimeter(solution(in.toList)))
  }

SetInt(
  """6
    |1 1
    |2 5
    |3 3
    |5 3
    |3 2
    |2 2
    |
  """.stripMargin)

}
