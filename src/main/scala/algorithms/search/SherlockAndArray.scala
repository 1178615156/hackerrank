package algorithms.search

import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/14.
  */
object SherlockAndArray {

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def split(list: List[Int]) = {
    def impl(list: List[Int], left: Int, right: Int): Boolean = {
      if (left == right - list.head)
        true
      else if (left > right - list.head)
        false
      else {
        impl(list.tail, left + list.head, right - list.head)
      }
    }
    impl(list,0,list.sum)
  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val data = 1 to n map { _ =>
      readListInt()
      readListInt()
    }
    val result = data map (e => split(e)) map {
      case true => "YES"
      case false => "NO"
    } mkString "\n"
    println(result)
  }


  SetInt(
    """2
      |3
      |1 2 3
      |4
      |1 2 3 3
      |
    """.stripMargin)
}
