package contests.week_21

import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/6/29.
  */
class Kangaroo$Test extends FunSuite {
  test("a") {
    import Kangaroo._
    assert(
      solution(
        Kangaroo(0, 3),
        Kangaroo(4, 2)
      )
    )


  }
  test("b"){
    import Kangaroo._
    assert(
      solution(
        Kangaroo(0, 2),
        Kangaroo(5, 3)
      )
    )
  }
}
