package algorithms.implementation

import utils.SetInt

/**
  * Created by yujieshui on 2016/3/7.
  */
object ExtraLongFactorials {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def factorial(bigInt: Int, rt: BigInt = 1): BigInt = {
    if (bigInt == 1)
      rt
    else factorial(bigInt - 1, rt * bigInt)
  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    println(factorial(n))
  }
  SetInt(
    """25
    """.stripMargin)
}
