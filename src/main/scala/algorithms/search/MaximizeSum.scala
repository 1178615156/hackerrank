package algorithms.search

import utils.SetInt

/**
  * Created by yuJieShui on 2016/5/14.
  */
object MaximizeSum {
  def mkSubArray(l: List[Int], rt: Seq[Seq[Int]] = Seq()): Seq[Seq[Int]] = {
    if (l.isEmpty)
      rt.filter(_.nonEmpty)
    else
      mkSubArray(l.tail, l.inits.toSeq ++ rt)
  }

  def maximizeSum(l: List[Int], n: Int) = {
    mkSubArray(l).map(_.sum % n).max
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val testSize :: Nil = readListInt()
    val data =
      1 to testSize map { _ =>
        val m :: n :: Nil = readListInt()
        val list = readListInt()
        list -> n
      }

    val result =
    data.map{
      case (l,n) => maximizeSum(l,n)
    }
    println(
      result.mkString("\n")
    )
  }
  SetInt(
    """1
      |5 7
      |3 3 9 9 5
    """.stripMargin)
}
