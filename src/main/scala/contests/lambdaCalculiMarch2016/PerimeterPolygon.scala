package contests.lambdaCalculiMarch2016

import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/25.
  */
object PerimeterPolygon {

  case class Paint(x: Int, y: Int)

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
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()

    val in =
    1 to n map { _ =>
      val x :: y :: Nil = readListInt()
      Paint(x, y)
    }

    println(perimeter(in))
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
