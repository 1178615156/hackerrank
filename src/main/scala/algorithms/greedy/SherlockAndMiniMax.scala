package algorithms.greedy

import utils.SetInt

/**
  * Created by yujieshui on 2016/3/15.
  */
object SherlockAndMiniMax {
  //  type Range = ???

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def minAbs(list: List[Int]) = {
    list.map(math.abs).min
  }

  def minAbs(list: List[Int], mod: Int) = {
    list.map(e => math.abs(e - mod)).min
  }

  def minimax(list: List[Int], range: Seq[Int]) = {
    range.map(e =>
      e -> minAbs(list, e)
    ).minBy(_._2)._1
  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val array = readListInt()
    val begin :: end :: Nil = readListInt()
    val result = minimax(array, begin to end filterNot  array.contains)
    println(result)
  }

  SetInt(
    """3
      |5 8 14
      |4 9
    """.stripMargin)
}
