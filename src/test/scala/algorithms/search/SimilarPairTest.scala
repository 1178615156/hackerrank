package algorithms.search

import org.scalatest.{FunSuite, WordSpec}
import SimilarPair._
/**
  * Created by yujieshui on 2017/6/27.
  */
class SimilarPairTest extends WordSpec {

  "solution" in {
    val n = 5
    val k = 2
    val data = Seq(
      Node(3, 2),
      Node(3, 1),
      Node(1, 4),
      Node(1, 5)
    )
    val result = SimilarPair.solution(n,k, data)
    println(result.mkString("\n"))
    assert(result.sum  === 4 )
  }
}
