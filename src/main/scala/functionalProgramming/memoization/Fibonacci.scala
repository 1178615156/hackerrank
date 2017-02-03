package functionalProgramming.memoization

/**
  * Created by yujieshui on 2017/2/2.
  */
object Fibonacci {

  def mkCache(max: Int, i: Int, result: Seq[BigInt]): Seq[BigInt] = {
    if(i > max) result.reverse.toVector
    else mkCache(max, i + 1, result.take(2).sum +: result)
  }

  def solution(seq: Seq[Int]) = {
    val max = seq.max
    val cache = mkCache(max, 2, Seq(BigInt(1), BigInt(0)))

    val rt = seq.foldLeft(Seq[BigInt]()) {
      case (result, i) =>
        cache(i) +: result
    }
    rt.reverse map (_ % (1E8.toInt + 7))
  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    val n :: Nil = readListInt()
    val seq = 1 to n map (_ => readListInt().head)
    val result = solution(seq)

    println(result.mkString("\n"))
  }

}
