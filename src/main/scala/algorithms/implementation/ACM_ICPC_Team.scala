package algorithms.implementation

import utils.SetInt

/**
  * Created by yujieshui on 2016/3/7.
  */
object ACM_ICPC_Team {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def topicNumber(per: Seq[Seq[Boolean]], rt: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    def impl(p: Seq[Boolean], op: Seq[Seq[Boolean]]): Seq[Int] =
      op.map(_ zip p count { case (o, p) => o | p })

    if (per.isEmpty)
      rt.reverse
    else
      topicNumber(per.tail, impl(per.head, per.tail) +: rt)
  }

  def team(data: Seq[Seq[Boolean]]) = {
    val allTopicNumber = topicNumber(data, Nil).flatten
    val maxNum = allTopicNumber.max

    maxNum -> (
      allTopicNumber count (_ == maxNum)
      )
  }


  def main(args: Array[String]) {
    val n :: m :: Nil = readListInt()
    val data = 1 to n map (_ => io.StdIn.readLine().map(_.toString.toInt) map {
      case 1 => true
      case 0 => false
    })
    val (num, person) = team(data)
    println(num)
    println(person)
  }

  SetInt(
    """4 5
      |10101
      |11100
      |11010
      |00101""".stripMargin)

}
