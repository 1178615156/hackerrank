package algorithms.greedy

object MarcsCakewalk {
  def solution(n: Int, cupcakes: Seq[Int]): Long = {
    val index = 0 until n
    val order = cupcakes.sorted.reverse
    val walks = index zip order map {
      case (i, c) =>
        c.toLong * (2L << i)
    }
    walks.sum
  }

  def readListInt(): List[Int] = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val cupcakes = readListInt()
    val result = solution(n, cupcakes)
    println(result)
  }
}
