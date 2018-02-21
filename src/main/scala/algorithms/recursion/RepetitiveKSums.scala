package algorithms.recursion

object RepetitiveKSums {

  def canDivide(k: Long, seq: Seq[Long]): Seq[Long] = {
    seq.filter { e =>(e / k) * k == e}.map(_ / k)
  }

  def solution(n: Long, k: Long, seq: Seq[Long]) = {
    seq.sorted

  }
}
