package algorithms.greedy

/**
  * Created by yujieshui on 2017/5/12.
  */
object MaxMin {
  def impl(seq: Seq[Int], k: Int): Seq[Int] = {
    if(seq.length < k)
      Nil
    else {
      val l = seq.take(k)
      (l.last - l.head) +: impl(seq.tail, k)
    }
  }

  def solution(seq: Seq[Int], k: Int) = {
    impl(seq.sorted.toVector, k).min
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val k :: Nil = readListInt()
    val data = 1 to n map (_ => readListInt().head)
    val result = solution(data, k)
    println(result)
  }
}
