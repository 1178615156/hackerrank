package algorithms.search

import org.scalatest.FunSuite
import MinimunLoss._

/**
  * Created by yujieshui on 2017/6/21.
  */
class MinimunLossTest extends FunSuite {

  test("0") {
    assert(solution("5 10 3".split(" ").toList.map(_.toLong)) === 2)
  }
  test("1") {
    assert(solution("20 7 8 2 5".split(" ").toList.map(_.toLong)) === 2)
  }
}
