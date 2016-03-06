package algorithms.implementation

import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/5.
  */
object CutAndSticks {

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def cut(l: Seq[Int], rt: Seq[Int] = Nil): Seq[Int] = {

    if (l.isEmpty)
      rt.reverse
    else
      {
        cut(
          l.map(_ - l.min).filter(_ > 0),
          l.size +: rt
        )
      }
  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val data = readListInt()
    val result = cut(data)
    val out = result mkString "\n"
    println(out)
  }

  SetInt(
    """8
      |1 2 3 4 3 3 2 1
    """.stripMargin)
}
