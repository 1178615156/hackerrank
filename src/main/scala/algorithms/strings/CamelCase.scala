package algorithms.strings

/**
  * Created by yujieshui on 2017/4/25.
  */
class CamelCase {

  def main(args: Array[String]): Unit = {
    val s = io.StdIn.readLine()
    val result = s.count(_.isUpper) + 1
    println(result)
  }
}
