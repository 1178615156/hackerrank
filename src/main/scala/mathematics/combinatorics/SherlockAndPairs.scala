package mathematics.combinatorics

/**
  * Created by yujieshui on 2017/3/8.
  */
object SherlockAndPairs {
  def solution(arr: Seq[Int]): Long = {
    arr.groupBy(e => e)
      .mapValues(_.size.toLong)
      .values
      .filter(_ > 1)
      .map(e => e * (e - 1)).sum
  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    val n :: Nil = readListInt()
    val data = 1 to n map { _ =>
      val m :: Nil = readListInt()
      readListInt()
    }
    println(

      data.map(solution(_)).mkString("\n")
    )
  }
}
