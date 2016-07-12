package algorithms.bitManipulation

import algorithms.bitManipulation.CountGame._
import algorithms.bitManipulation.CountGame.GameUser._
import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/7/12.
  */
class CountGame$Test extends FunSuite {

  test("a") {
    assert(solution(2) === Louise)
    assert(solution(3) === Louise)
    assert(solution(4) === Richard)
    assert(solution(5) === Louise)
    assert(solution(6) === Richard)
    assert(solution(7) === Richard)
  }
}
