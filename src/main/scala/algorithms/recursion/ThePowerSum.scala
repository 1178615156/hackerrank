package algorithms.recursion


object ThePowerSum {

  import scala.collection.mutable

  def pow(n: Long, p: Int): Long = {
    if(p == 1) n
    else n * pow(n, p - 1)
  }

  def impl(x: Long, numbers: Set[Long])(cache: mutable.Map[Set[Long], Set[Set[Long]]]): Set[Set[Long]] = {
    if(x == 0)
      Set(Set())
    else if(cache.contains(numbers)) cache(numbers)
    else {
      val result = numbers.filter(_ <= x).flatMap { num =>
        impl(x - num, numbers.filter(_ != num))(cache).map(_ + num)
      }
      cache += numbers -> result
      result
    }
  }

  def solution(x: Int, p: Int) = {
    impl(x, 1 to math.sqrt(x).toInt map (n => pow(n, p)) toSet)(mutable.Map())
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val x :: Nil = readListInt()
    val n :: Nil = readListInt()
    val result = solution(x, n)
    println(result.size)
  }
}
