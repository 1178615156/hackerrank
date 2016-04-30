package contests.lambdaCalculiMarch2016

import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/26.
  */
object ConcavePolygon {

  case class Paint(x: Double, y: Double) {
    def *(paint: Paint) = Paint(this.x * paint.x, this.y * paint.y)

    def -(paint: Paint) = Paint(this.x - paint.x, this.y - paint.y)
  }

  type Paints = Seq[Paint]

  case class Line(head: Paint, tail: Paint)

  def paints2Line(paints: Seq[Paint]): Seq[(Line, Line)] = {

    def skipPaint(paints: Paints, doNum: Int, nowNum: Int = 0, result: Seq[(Line, Line)] = Nil): Seq[(Line, Line)] = {
      if (nowNum >= doNum)
        result
      else {
        skipPaint(paints.tail, doNum, nowNum + 1,
          (Line(paints(1), paints(0)), Line(paints(1), paints(2))) +: result)
      }
    }

    skipPaint(paints ++ paints.take(2), paints.size)
  }


  def isConcave(paints: Seq[Paint]) = {
    paints2Line(paints) map {
      case (left, right: Line) =>
        val resultPaint =
          (left.tail - left.head) * (right.tail - left.tail)
        resultPaint.x + resultPaint.y
    } forall (_ > 0)
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val in = 1 to n map (_ => {
      val x :: y :: Nil = readListInt()
      Paint(x, y)
    })
    println(isConcave(in) match {
      case true => "YES"
      case false => "NO"
    })
  }

  SetInt(
    """4
      |0 0
      |0 1
      |1 1
      |1 0
      |
    """.stripMargin)
}
