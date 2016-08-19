package algorithms.gameTheory

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/8/12.
  */
class TowerBreakersRevisitedTest extends FunSuite {

  import TowerBreakersRevisited._
  test("solution"){
    assert(solution(Seq(1,2)) === "1")
    assert(solution(Seq(1,2,3)) === "2")
  }
}
