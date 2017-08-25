package algorithms.greedy

object PryankaAndToys {
  def impl(l: Seq[Int], acc: Int): Int = {
    if(l.isEmpty) acc
    else {
      val a = l.head
      val (canBay, other) = l.span(_ <= a + 4)
      impl(other, acc + 1)
    }
  }

  def solution(l: Seq[Int]): Int = {
    impl(l.sorted, 0)
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val data = readListInt()
    val result = solution(data)
    println(
      result
    )
  }
}
