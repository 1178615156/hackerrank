package algorithms.implementation

import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/5.
  */
object FindDigits {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def digits(n: Int) = {
    n.toString map (_.toString.toInt) filter(_ != 0) count (n % _ == 0)
  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val data = 1 to n map (_ => readListInt().head)
    val result = data map digits
    val out = result mkString "\n"
    println(out)
  }


  SetInt(
    """2
      |12
      |1012
    """.stripMargin)
}
