package algorithms.greedy

import utils.SetInt

/**
  * Created by yujieshui on 2016/3/15.
  */
object GridChallenge {

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def listIsSort(l: Seq[Int], now: Int = Int.MinValue): Boolean = {
    if (l.isEmpty)
      true
    else if (now > l.head)
      false
    else
      listIsSort(l.tail, l.head)
  }

  def bool2YesNo(boolean: Boolean) = boolean match {
    case true => "YES"
    case false => "NO"
  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val data = 1 to n map (_ => {
      val t :: Nil = readListInt()
      1 to t map (_ =>
        io.StdIn.readLine().toList.map(_.toInt)
        )
    })
    val result =
      data.map(_.map(_.sorted).transpose.forall(e => listIsSort(e)))
        .map(bool2YesNo).mkString("\n")
    println(result)
  }

  SetInt(
    """1
      |5
      |ebacd
      |fghij
      |olmkn
      |trpqs
      |xywuv
      |
    """.stripMargin)
}
