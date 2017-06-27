package algorithms.search

/**
  * Created by yujieshui on 2017/6/21.
  */
object Pairs {
  def solution(k: Int, seq: Seq[Int]) = {
    val set = seq.toSet
    seq.count(e => set.contains(e - k))
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: k :: Nil = readListInt()
    val seq = readListInt()
    val result = solution(k, seq)
    println(result)

  }
}
