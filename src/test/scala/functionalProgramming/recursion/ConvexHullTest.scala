package functionalProgramming.recursion

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/7/19.
  */
class ConvexHullTest extends FunSuite {

  import ConvexHull._

  test("solution") {
    val result = solution(List(
      Paint(1, 1),
      Paint(2, 5),
      Paint(3, 3),
      Paint(5, 3),
      Paint(3, 2),
      Paint(2, 2)
    ))
    //    assert(result.toSet === Set(Paint(1, 1), Paint(2, 5), Paint(5, 3)))

    println(result.mkString("\n"))
  }

  test("solution 4") {
    val result = solution(List(
      Paint(x = 3.0, y = 2.0),
      Paint(1, 1),
      Paint(2, 5),
      Paint(5, 3)
    ))
    println(result.mkString("\n"))
  }
  test("solution 2") {
    val result = solution(List(
      Paint(1, 1),
      Paint(-1, -1),
      Paint(1, -1),
      Paint(-1, 1),
      Paint(0, 0)
    ))
    println(result.mkString("\n"))
    assert(result === Set(Paint(1, 1), Paint(-1, -1), Paint(1, -1), Paint(-1, 1)))
  }

  test("solution 3") {
    val result = solution(List(
      Paint(1, 1), Paint(2, 5), Paint(5, 3)
    ))
    assert(result.toSet === Set(Paint(1, 1), Paint(2, 5), Paint(5, 3)))
  }
}
