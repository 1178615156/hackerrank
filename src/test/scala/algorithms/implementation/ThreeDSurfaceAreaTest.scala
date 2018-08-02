package algorithms.implementation

import org.scalatest.FunSuite

class ThreeDSurfaceAreaTest extends FunSuite {

  import ThreeDSurfaceArea._

  test("test") {
    assert(solution(1, 1, List(List(1))).sum === 6)
  }
  test("test 2") {
    val result  = solution(3, 3, List(
      List(1, 3, 4),
      List(2, 2, 3),
      List(1, 2, 4)
    ))
    println(result.sum)

  }
}
