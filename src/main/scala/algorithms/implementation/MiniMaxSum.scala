package algorithms.implementation

/**
  * Created by yujieshui on 2017/2/17.
  */
object MiniMaxSum {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toLong)

  def main(args: Array[String]): Unit = {
    val data = readListInt()
    val l = data.sorted
    val max = l.tail.sum
    val min = l.init.sum
    println(s"$min $max")
  }
}
