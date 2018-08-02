package functionalProgramming.ad_hoc

object PuzzleAndPC {

  case class Point(x: Int, y: Int)

  case class Tromino(a: Point, b: Point, c: Point) {
    def move(x: Int, y: Int) = Tromino(Point(a.x + x, a.y + y), Point(b.x + x, b.y + y), Point(c.x + x, c.y + y))
  }

  val fourPoint = List(Point(1, 1), Point(1, 2), Point(2, 1), Point(2, 2))

  var cache = Map[(Int, Point), Seq[Tromino]]()

  def solution(n: Int, point: Point): Seq[Tromino] = {
    val result =
      if(n == 1) {
        val a :: b :: c :: Nil = fourPoint.filter(_ != point)
        Seq(Tromino(a, b, c))
      } else {
        val size = 2 << (n - 2)
        lazy val leftUp = solution(n - 1, Point(size, size))
        lazy val rightUp = solution(n - 1, Point(size, 1)).map(_.move(0, size))
        lazy val leftDown = solution(n - 1, Point(1, size)).map(_.move(size, 0))
        lazy val rightDown = solution(n - 1, Point(1, 1)).map(_.move(size, size))

        lazy val miss_leftUp = solution(n - 1, point)
        lazy val miss_rightUp = solution(n - 1, point.copy(y = point.y - size)).map(_.move(0, size))
        lazy val miss_leftDown = solution(n - 1, point.copy(x = point.x - size)).map(_.move(size, 0))
        lazy val miss_rightDown = solution(n - 1, Point(x = point.x - size, y = point.y - size)).map(_.move(size, size))

        lazy val leftUp_point = Tromino(Point(size, size + 1), Point(size + 1, size), Point(size + 1, size + 1))
        lazy val rightUp_point = Tromino(Point(size, size), Point(size + 1, size), Point(size + 1, size + 1))
        lazy val leftDown_point = Tromino(Point(size, size), Point(size, size + 1), Point(size + 1, size + 1))
        lazy val rightDown_point = Tromino(Point(size, size), Point(size, size + 1), Point(size + 1, size))

        (point.x <= size, point.y <= size) match {
          case (true, true)   => leftUp_point +: (miss_leftUp ++ rightUp ++ leftDown ++ rightDown)
          case (true, false)  => rightUp_point +: (leftUp ++ miss_rightUp ++ leftDown ++ rightDown)
          case (false, true)  => leftDown_point +: (leftUp ++ rightUp ++ miss_leftDown ++ rightDown)
          case (false, false) => rightDown_point +: (leftUp ++ rightUp ++ leftDown ++ miss_rightDown)
        }
      }
    result
  }

  def tromionToString(tromino: Tromino) = {
    import tromino._
    s"${a.x} ${a.y} ${b.x} ${b.y} ${c.x} ${c.y}"
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val x :: y :: Nil = readListInt()

    println(solution(n, Point(x, y)).map(tromionToString).mkString("\n"))
  }
}
