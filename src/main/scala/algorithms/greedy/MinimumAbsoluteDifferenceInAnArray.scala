package algorithms.greedy

/**
  * Created by yujieshui on 2017/5/12.
  */
class MinimumAbsoluteDifferenceInAnArray {

  def solution(seq: Seq[Int]) = {
    val l = seq.sorted
    (l zip l.tail map { case (a, b) => math.abs(a - b) }).min
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val data = readListInt()
    println(solution(data))
  }
}
