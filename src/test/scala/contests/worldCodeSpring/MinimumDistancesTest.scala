package contests.worldCodeSpring

import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/6/26.
  */
class MinimumDistancesTest extends FunSuite {

  import MinimumDistances._

  test("testSolution") {
    println(
      solution(
        Seq(7, 1, 3, 4, 1, 7), 0, Map(), Map()
      )
    )
  }

}
