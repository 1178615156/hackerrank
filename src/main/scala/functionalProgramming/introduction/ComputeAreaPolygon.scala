package functionalProgramming.introduction

/**
  * Created by yujieshui on 2016/7/18.
  */
object ComputeAreaPolygon {

  case class Paint(x: Int, y: Int)

  def area(paints: Seq[Paint]) = {
    val ((xy, yx), _) = paints.foldLeft(((0, 0), paints.last)) {
      case (((xy, yx), head), tail) =>
        ((xy + head.x * tail.y, yx + head.y * tail.x), tail)
    }
    math.abs(xy - yx).toDouble / 2
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val in =
      1 to n map { _ =>
        val x :: y :: Nil = readListInt()
        Paint(x, y)
      }
    println(area(in))
  }
}
