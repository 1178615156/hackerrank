package algorithms.implementation


object AbsolutePermutation {

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

  case class Result(index: Int, value: Int)

  def isOut(i: Int, n: Int) = (1 <= i && i <= n) == false

  def impl2(n: Int, k: Int, currentElement: Int, historyList: List[Result], set: Set[Int]): List[Result] = {
    if(set.size == n) historyList
    else if(historyList.exists(_.index == currentElement)) impl2(n, k, (currentElement + 1) % (n + 1), historyList, set)
    //    else if(set.contains(currentElement)) impl2(n, k, (currentElement + 1) % (n + 1), historyList, set)
    else {
      val i = currentElement
      val l = i - k
      val r = i + k
      lazy val rr =
        if(set.contains(r)) Nil
        else if(isOut(r + k, n)) impl2(n, k, i + 1, Result(i, r) :: historyList, set + r)
        else impl2(n, k, r + k, Result(i, r) :: historyList, set + r)
      lazy val ll =
        if(set.contains(l)) Nil
        else if(isOut(l - k, n)) impl2(n, k, i + 1, Result(i, l) :: historyList, set + l)
        else impl2(n, k, l - k, Result(i, l) :: historyList, set + l)

      (1 <= l && l <= n, 1 <= r && r <= n) match {
        case (true, true)  =>
          if(set.contains(l) && set.contains(r)) Nil
          else if(set.contains(l)) rr
          else if(set.contains(r)) ll
          else Nil
        case (false, true) => rr
        case (true, false) => ll

        case _ => Nil
      }
    }
  }

  def solution2(n: Int, k: Int) = {
    impl2(n, k, 1, Nil, Set()).sortBy(_.index).map(_.value) match {
      case Nil => List(-1)
      case e   => e
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
    val result = cases map (l => solution2(l(0), l(1)))
    println(result.map(_.mkString(" ")).mkString("\n"))
  }
}
