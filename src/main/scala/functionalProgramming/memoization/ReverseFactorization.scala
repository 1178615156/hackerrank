package functionalProgramming.memoization

/**
  * Created by yujieshui on 2017/2/2.
  */

object ReverseFactorization {

  def impl_opt(ns_acc: Seq[(Int, Seq[Int])], arr: Seq[Int], history: Set[Int]): Seq[Seq[Int]] = {
    val next =
      ns_acc.flatMap { case (n, acc) =>
        arr
          .collect { case i if n % i == 0 => n / i }
          .filter(_ != 0)
          .filter(e => !history.contains(e))
          .map { divisor => divisor -> ((n / divisor) +: acc) }
      }

    if(next.isEmpty) Seq(Seq())
    else if(next.exists(_._1 == 1)) next.collect { case (1, acc) => acc }
    else impl_opt(next, arr, history ++ next.map(_._1))
  }

  def notDiv(arr: Seq[Int]): Seq[Int] = {
    arr.filter(e=>arr.filter(_ != e ).forall(e2=>e2 % e !=0 && e % e2 !=0))
  }

  def dropNotDiv(n: Int, arr: Seq[Int], result: Seq[Int]): (Int, Seq[Int]) = {
    if(arr.isEmpty) n -> result
    else if(n % arr.head == 0) dropNotDiv(n / arr.head, arr, arr.head +: result)
    else dropNotDiv(n, arr.tail, result)
  }

  def solution(n: Int, arr: Seq[Int]): Seq[Int] = {
    val (rn, racc) = dropNotDiv(n, notDiv(arr).filter(n % _ == 0), Seq())
    val result =
        impl_opt(Seq(rn -> Seq()), arr.distinct.filter(n % _ == 0), Set(n))

    if(!result.exists(_.nonEmpty) && rn != 1)
      Seq(-1)
    else
      result.map(_ ++ racc).map(_.sorted.foldLeft(Seq(1)) { case (r, i) => (r.head * i) +: r }.reverse).minBy(_.sum)
  }

  def main(args: Array[String]): Unit = {

    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    val n :: _ :: Nil = readListInt()
    val seq = readListInt()
    println(
      solution(n, seq).mkString(" ")
    )
  }
}
