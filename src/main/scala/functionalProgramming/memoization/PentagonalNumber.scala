package functionalProgramming.memoization

/**
  * Created by yujieshui on 2016/7/25.
  */
object PentagonalNumber {
  def cacheResult(n: Int): Map[Int, BigInt] = {
    (1 to n).foldLeft(Map(1 -> BigInt(1), 2 -> BigInt(5))) {
      case (cache, i) => cache.get(i) match {
        case Some(_) => cache
        case None    =>
          val r = cache(i - 1) + ((BigInt(3) * i) - 2)
          cache + (i -> r)
      }
    }
  }

  def solution(i: Int, map: Map[Int, BigInt]): BigInt = map(i)

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    val n :: Nil = readListInt()

    val data =       1 to n map (_ => readListInt().head)
    val cacheMap = cacheResult(data.max)
    val out = data map ( i=> solution(i,cacheMap))
    println(
      out mkString "\n"
    )
  }
}
