package algorithms.gameTheory

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/8/11.
  */
class ChessboardGameTest extends FunSuite {

  import algorithms.gameTheory.ChessboardGame._

  test("solution") {
    assert(solution(Point(5, 2), Play.First) === Play.Second)
    assert(solution(Point(5, 3), Play.First) === Play.First)
    assert(solution(Point(8, 8), Play.First) === Play.First)
  }
}
