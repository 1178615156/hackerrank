package functionalProgramming.memoization

/**
  * Created by yujieshui on 2017/2/7.
  */
object DifferentWays {

  def solution(n: Int, k: Int) = {
    combination(n, k) % (1E7.toInt + 7)
  }

  def combination(n: Long, m: Long): BigInt =
    (0L until m map (n - _) map (BigInt(_))).product / (1L to m).map(BigInt(_)).product

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val data = 1 to n map (_ => readListInt())

    println(
      data.map(e => solution(e(0), e(1))).mkString("\n")
    )
  }
}
