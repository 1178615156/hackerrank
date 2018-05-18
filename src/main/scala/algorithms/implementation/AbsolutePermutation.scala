package algorithms.implementation


object AbsolutePermutation {

  def candidates(n: Int, k: Int, i: Int): List[Int] = {
    val x_l = i - k
    val x_r = i + k

    (1 <= x_l && x_l <= n, 1 <= x_r && x_r <= n) match {
      case (true, true)  => List(x_l, x_r)
      case (false, true) => List(x_r)
      case (true, false) => List(x_l)
      case _             => Nil
    }
  }

  def impl(n: Int, k: Int, workList: List[Int], historyList: List[Int], set: Set[Int] = Set()): List[Int] = {
    workList match {
      case Nil          => historyList
      case head :: tail =>
        val i = head
        val l = i - k
        val r = i + k
        lazy val a = if(set.contains(l)) Nil else impl(n, k, tail, l :: historyList, set + l)
        lazy val b = if(set.contains(r)) Nil else impl(n, k, tail, r :: historyList, set + r)
        (1 <= l && l <= n, 1 <= r && r <= n) match {
          case (true, true)  => if(a.nonEmpty) a else b
          case (false, true) => b
          case (true, false) => a
          case _             => Nil
        }
    }
  }

  def solution(n: Int, k: Int) =
    if(k == 0)
      (1 to n).toList
    else if(n % 2 == 1)
      List(-1)
    else if(k > n / 2)
      List(-1)
    else if((n / k % 2 != 0) || (n % k != 0))
      List(-1)
    else
      impl(n, k, 1 to n toList, Nil) match {
        case Nil => List(-1)
        case e   => e.reverse
      }


  def readListInt() = scala.io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val cases = 1 to n map (_ => readListInt())
    val result = cases map (l => solution(l(0), l(1)))
    println(result.map(_.mkString(" ")).mkString("\n"))
  }
}
