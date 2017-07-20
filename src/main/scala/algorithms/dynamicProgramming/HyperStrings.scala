package algorithms.dynamicProgramming

/**
  * Created by yujieshui on 2017/7/20.
  */
object HyperStrings {

  def superStringLen(seq: Seq[String]): Map[Int, Int] =
    seq.map(_.length).groupBy(e => e).mapValues(_.size)


  def decompositionCache(n: Int, m: Int, cache: Map[Int, Set[Seq[Int]]]): Map[Int, Set[Seq[Int]]] = {
    if(cache.contains(m)) cache
    else if(n > m) cache
    else if(cache.contains(n)) decompositionCache(n + 1, m, cache)
    else {
      val r =
        (1 to n map (n - _) drop(n/2-1)) flatMap { i =>
          cache(i).map(e => ((n - i) +: e).sorted)
        }
      decompositionCache(n + 1, m, cache +(n -> r.toSet))
    }
  }

  def decomposition(n: Int): Seq[Seq[Int]] = {
    if(n == 0) List(List())
    else
      1 to n map (n - _) map { i =>
        decomposition(i).map(i +: _)
      } flatMap (e => e)
  }

  def decomposition(base: Seq[Int], n: Int): Option[Seq[Seq[Int]]] = {
    if(n == 0) Some(List(List()))
    else if(base.isEmpty) None
    else Some(
      1 to n filter (_ <= base.head) map (n - _) map { i =>
        decomposition(base.filter(_ <= i), i).map(_.map((n - i) +: _))
      } collect { case Some(x) => x } flatMap (e => e)
    )

  }

  def solution(n: Int, m: Int, set: Seq[String]) = {

  }
}
