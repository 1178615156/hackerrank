package algorithms.search

import scala.util.Random

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
    val result = SimilarPair.solution(n, k, data)
    assert(result === 4)
  }

  "group" in {
    import org.scalameter
    val r = new Random(111)
    val time = scalameter.measure {
      val node =
        1 to 100000 map (_ => Node(
          r.nextInt(100000),
          r.nextInt(100000)
        ))
      val x = node.groupBy(_.parent).mapValues(_.map(_.value))
      println(x.mapValues(_.length).values.groupBy(e => e).mapValues(_.size))
    }
    println(time)
  }
}
