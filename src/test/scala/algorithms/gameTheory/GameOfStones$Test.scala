package algorithms.gameTheory

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/8/10.
  */
class GameOfStones$Test extends FunSuite {

  import GameOfStones._


  test("a") {
    assert(solution(1, Play.First) === Play.Second)
    assert(solution(2, Play.First) === Play.First)
    assert(solution(3, Play.First) === Play.First)
    assert(solution(4, Play.First) === Play.First)
    assert(solution(5, Play.First) === Play.First)
    assert(solution(6, Play.First) === Play.First)
    assert(solution(7, Play.First) === Play.Second)
    assert(solution(10, Play.First) === Play.First)
  }
  test("10") {
    println(solution(10, Play.First))
    assert(solution(10, Play.First) === Play.First)

  }

  test("time") {
    import org.scalameter._
    val time =
      config().withWarmer(Warmer.Zero).measure(
        solution(50, Play.First)
      )
    println(time)
  }
}
