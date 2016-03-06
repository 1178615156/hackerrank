package algorithms.implementation

import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/5.
  */
object UtopianTree {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  type Height = Int

  def growth(year: Int, height: Height): Height = {
    year match {
      case 0 => height
      case 1 => height * 2
      case 2 => height * 2 + 1
      case e => growth(e - 2, height * 2 + 1)
    }

  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()

    val data = 1 to n map (_ => readListInt().head)
    val result = data map (growth(_, 1))
    val out = result mkString "\n"
    println(out)
  }

  SetInt(
    """3
      |0
      |1
      |4
    """.stripMargin)
}
