package algorithms.implementation

import org.scalatest.FunSuite

class ClimbingTheLeaderboardTest extends FunSuite {
  test("case 1") {
    import scala.collection.Searching._
    val sources = "100 100 50 40 40 20 10".split(" ").toList.map(_.toInt).distinct.reverse
    println(sources.search(5))
    println(sources.search(25))
    println(sources.search(50))
  }
}
