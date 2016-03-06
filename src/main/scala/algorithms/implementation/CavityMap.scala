package algorithms.implementation

import scala.collection.immutable
import scala.collection.immutable.IndexedSeq

import utils.SetInt


/**
  * Created by yuJieShui on 2016/3/6.
  */
object CavityMap {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  type Matrix = Seq[Seq[Int]]
  val X = Int.MaxValue

  def sunIndex(n: Int) = {
    val lineIndex = 1 to (n - 2)
    val index = lineIndex map (x => lineIndex.map(y => x -> y))
    index
  }

  def indexOfMaxNear(dimension: Int, matrix: Matrix): IndexedSeq[(Int, Int)] = {
    val index = sunIndex(dimension)
    index.flatten(_.filter {
      case (x, y) =>
        val paint = matrix(x)(y)
        def up = matrix(x - 1)(y)
        def down = matrix(x + 1)(y)
        def lift = matrix(x)(y - 1)
        def right = matrix(x)(y + 1)

        paint > up &&
          paint > down &&
          paint > lift &&
          paint > right
    })
  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val matrix: Matrix = 1 to n map (_ => io.StdIn.readLine().map(_.toString.toInt))

    val needUpdateIndex = indexOfMaxNear(n, matrix)

    val rt = needUpdateIndex.foldLeft(matrix) {
      case (matrix, (x: Int, y: Int)) =>
        matrix.updated(x, matrix(x).updated(y, X))
    }
    val out =
      rt.map(_.map(e => if (e == X) "X" else e.toString).mkString("")).mkString("\n")
    println(out)
  }

  SetInt(
    """4
      |1112
      |1912
      |1892
      |1234
    """.stripMargin)
}
