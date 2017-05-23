package algorithms.search

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2017/5/22.
  */
class GridlandMetroTest extends FunSuite {

  import GridlandMetro._

  test("a") {
    val result = solution(1, 10, Seq(
      Train(1, 1, 5)
      , Train(1, 5, 6)
    ))
    assert(result == 4)

  }

  test("b") {
    val result = solution(2, 10, Seq(
      Train(1, 1, 2)
      , Train(1, 3, 4)
      , Train(1, 4, 4)
      , Train(1, 4, 4)
      , Train(1, 2, 4)
      , Train(1, 4, 5)
      , Train(1, 8, 8)
      , Train(1, 9, 9)
      , Train(1, 10, 10)

      , Train(2, 4, 4)
    ))
    assert(result == 11)

  }
}
