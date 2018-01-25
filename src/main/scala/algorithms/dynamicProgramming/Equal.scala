package algorithms.dynamicProgramming

object Equal {
  def give(seq: List[Long], plus: Long = 0L, acc_rt: Long = 0L): Long = seq match {
    case Nil             => acc_rt
    case a :: Nil        => acc_rt
    case a :: b :: other =>
      val diff = b - a + plus

      val five = diff / 5
      val five_rest = diff % 5

      val two = five_rest / 2
      val two_rest = five_rest % 2

      val one = two_rest
      val result = five + two + one

      give((b + plus) :: other, plus + diff, acc_rt + result)
  }

  def solution(seq: Seq[Long]) = {
    give(seq.sorted.toList)
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toLong)

  def main(args: Array[String]): Unit = {
    val t :: Nil = readListInt()

    val data = 1L to t map (_ => {
      readListInt()
      readListInt()
    })
    val result = data.map(solution)
    println(result.mkString("\n"))
  }
}
