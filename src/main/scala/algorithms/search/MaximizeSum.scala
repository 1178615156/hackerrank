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

  def remainder(n:Int,seq:Seq[Int]):Seq[Int] = ???

  def groupInitBy(n:Int,seq:Seq[Int]):Seq[Seq[Int]] = ???

  def solution(l: List[Int], n: Int) :Int = {
    groupInitBy(n,remainder(n,l)).map(_.sum).max
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
      case (l,n) => solution(l,n)
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
