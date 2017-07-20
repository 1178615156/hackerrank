package algorithms.dynamicProgramming

/**
  * Created by yujieshui on 2017/7/20.
  */
object HyperStrings {

  def superStringLen(seq: Seq[String]): Map[Int, Int] =
    seq.map(_.length).groupBy(e => e).mapValues(_.size)

  def decomposition(base: Seq[Int], n: Int) = {


  }

  def solution(n: Int, m: Int, set: Seq[String]) = {

  }
}
