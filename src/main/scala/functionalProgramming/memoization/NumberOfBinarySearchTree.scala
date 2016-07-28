package functionalProgramming.memoization

/**
  * Created by yujieshui on 2016/7/25.
  */
object NumberOfBinarySearchTree {


  def cacheMap(i: Int, initMap: Map[Int, BigInt] = Map(0 -> BigInt(0), 1 -> BigInt(1))): Map[Int, BigInt] = {
    val resultMap =
      (1 to i).foldLeft(initMap) { case (cache, e) =>
        cache.get(e) match {
          case Some(x) => cache
          case None    =>
            val result = (1 to e map (i => (i - 1, e - i)) map {
              case (left, right) =>
                val left_value = cache(left)
                val right_value = cache(right)
                if (left_value == 0 || right_value == 0) left_value + right_value else left_value * right_value
            }).sum
            cache + (e -> result)
        }
      }
    resultMap
  }

  def solution(i: Int, resultMap: Map[Int, BigInt]): BigInt = resultMap(i) % BigInt(100000007)


  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val data =
      (1 to n map { _ => readListInt().head })

    val _cacheMap = cacheMap(data.max)
    val out = data map (i => solution(i , _cacheMap))
    println(
      out mkString "\n"
    )
  }
}
