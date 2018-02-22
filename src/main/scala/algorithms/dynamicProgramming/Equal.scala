package algorithms.dynamicProgramming

object Equal {

  def give(seq: Seq[Long], min: Long) = {
    seq.map { e =>
      val diff = e - min

      val five = diff / 5
      val five_rest = diff % 5

      val two = five_rest / 2
      val two_rest = five_rest % 2

      val one = two_rest
      val result = five + two + one

      result
    }.sum
  }

  def solution(seq: Seq[Long]) = {
    val min = seq.min
    (0 until 5).map(base => give(seq, min - base)).min
  }

  def readListInt() = scala.io.StdIn.readLine().split(" ").toList.map(_.toLong)

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
