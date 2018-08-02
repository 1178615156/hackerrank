package algorithms.implementation


object ThreeDSurfaceArea {

  import scala.util.Try

  type Stack = Int
  type Line = Seq[Stack]
  type Matrix = Seq[Line]
  type Point = (Int, Int)

  def lookAround(matrix: Matrix, original: Point): Stack = {
    val (i, j) = original

    val value: Stack = matrix(i)(j)
    val up = (i + 1, j)
    val down = (i - 1, j)
    val left = (i, j - 1)
    val right = (i, j + 1)

    val result = for {
      checkPoint <- 1 to value
      (x, y) <- List(up, down, left, right)
    } yield {
      Try(matrix(x)(y))
        .map(e => if(e >= checkPoint) 0 else 1).getOrElse(1)
    }

    result.sum
  }

  def solution(h: Int, w: Int, matrix: Matrix) = {
    val result = for {
      i <- 0 until h
      j <- 0 until w
    } yield {
      if(matrix(i)(j) == 0)
        0
      else {
        val around = lookAround(matrix, (i, j))
        around + 2
      }
    }

    result

  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val h :: w :: Nil = readListInt()
    val data = 1 to h map (_ => readListInt())
    val result = solution(h, w, data.toList).sum
    println(result)
  }
}
