package algorithms.implementation

import utils.SetInt

/**
  * Created by yuJieShui on 2016/5/1.
  */
object NewYearChaos {

  def checkChaos(list: List[Int]) = {
    def impl(list: List[Int], number: Int, count: Int): Option[Int] = list match {
      case Nil                        => Some(count)
      case `number` :: tail           => impl(tail, number - 1, count)
      case a :: `number` :: tail      => impl(a :: tail, number - 1, count + 1)
      case a :: b :: `number` :: tail => impl(a :: b :: tail, number - 1, count + 2)
      case _                          => None
    }
    impl(list.reverse, list.size, 0)
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val testSize :: Nil = readListInt()
    val indata = 1 to testSize map { _ =>
      readListInt()
      readListInt()
    }
    val result = indata map (e => checkChaos(e)) map {
      case Some(i) => i.toString
      case None    => "Too chaotic"
    }
    println(result mkString "\n")
  }

  SetInt(
    """2
      |5
      |2 1 5 3 4
      |5
      |2 5 1 3 4
    """.stripMargin)
}
