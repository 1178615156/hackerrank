package algorithms.gameTheory

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/8/10.
  */
class TowerBreakersTest extends FunSuite {

  import algorithms.gameTheory.TowerBreakers._

  test("solution") {
    assert(solution(2, 2, Play.First) === Play.Second)
    assert(solution(1, 4, Play.First) === Play.First)
  }
}
