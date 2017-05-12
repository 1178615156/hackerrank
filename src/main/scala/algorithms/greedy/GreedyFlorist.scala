package algorithms.greedy

/**
  * Created by yujieshui on 2017/5/12.
  */
object GreedyFlorist {
  def price(x: Int, c: Int) = (x + 1) * c


  def impl(seq: Seq[Int], k: Int, x: Int): Int = {
    if(seq.isEmpty) 0
    else {
      val n = seq.take(k).map(c => price(x, c)).sum
      n + impl(seq.drop(k), k, x + 1)
    }
  }

  def solution(seq: Seq[Int], k: Int) = {
    val l = seq.sorted.reverse
    impl(l, k, 0)
  }
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: k :: Nil = readListInt()
    val data = readListInt()
    val result = solution(data,k)
    println(result)
  }
}
