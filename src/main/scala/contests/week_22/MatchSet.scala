package contests.week_22

import utils.SetInt

/**
  * Created by yujieshui on 2016/8/11.
  */
object MatchSet {
  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toLong)
    val _ :: Nil = readListInt()
    val x = readListInt().sorted
    val y = readListInt().sorted
    val (n: List[Long], p: List[Long]) =
      x zip y map { case (l, r) => l - r } filter (_ != 0) partition (_ < 0)
    val result: Long =
      if (n.sum + p.sum ==0) p.sum else -1L

    println(result)
  }


  SetInt(
    """3
      |1 2 3
      |-1 4 3
      |
    """.stripMargin)
}
