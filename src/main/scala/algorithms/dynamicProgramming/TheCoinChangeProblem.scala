package algorithms.dynamicProgramming

object TheCoinChangeProblem {

  type Coin = Int
  type Coins = Seq[Coin]

  var cache = Map[(Int, Coins), Long]()

  def impl(m: Int, coins: Coins): Long = {
    if(m == 0) 1L
    else if(m < 0) 0L
    else if(coins.isEmpty) 0L
    else if(cache.contains(m -> coins)) cache.apply(m -> coins)
    else {
      val coin = coins.head
      val combination = 0 to (m / coin) map (_ * coin)
      val combinationResult = combination map (other => impl(m - other, coins.tail))
      val result = combinationResult.sum
      if(result > 0)
        cache += ((m -> coins) -> result)
      result
    }
  }

  def solution(m: Int, coins: Coins) = {
    impl(m, coins)
  }


  def readListInt() = scala.io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val m :: n :: Nil = readListInt()
    val coins = readListInt()

    val result = solution(m, coins)
    println(result)
  }
}
