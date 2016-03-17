package contests.accelHack

import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/15.
  */
object ShortenPath {

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  type Paint = (Int, Int)

  def direction2finalPaint(s: Seq[Char], x: Int = 0, y: Int = 0): Paint = {
    if (s.isEmpty)
      (x, y)
    else
      s.head match {
        case 'N' => direction2finalPaint(s.tail, x, y + 1)
        case 'E' => direction2finalPaint(s.tail, x + 1, y)
        case 'S' => direction2finalPaint(s.tail, x, y - 1)
        case 'W' => direction2finalPaint(s.tail, x - 1, y)
      }
  }

  def paint2direction(paint: Paint): String = {
    val (x, y) = paint
    val dx =
      if (x > 0)
        1 to x map (_ => "E")
      else
        1 to math.abs(x) map (_ => "W")

    val dy =
      if (y > 0)
        1 to y map (_ => "N")
      else
        1 to math.abs(y) map (_ => "S")

    (dx ++ dy) mkString ""
  }

  def main(args: Array[String]) {
    val strangerDirection = io.StdIn.readLine()
    val result =
      paint2direction(direction2finalPaint(strangerDirection)).sorted

    println(result)
  }
  List

  SetInt(
    """NNNSSSS
      |
    """.stripMargin)
}
