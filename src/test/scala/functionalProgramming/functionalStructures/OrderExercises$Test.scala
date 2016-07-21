package functionalProgramming.functionalStructures

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/7/21.
  */
class OrderExercises$Test extends FunSuite {

  import OrderExercises._

  test("spilt sub array") {
    val data = Seq(1, 2, 3, 4)
    assert(spiltSubArray(data, 1) === Seq(Seq(1), Seq(2), Seq(3), Seq(4)))
    assert(spiltSubArray(data, 2) === Seq(Seq(1, 2), Seq(2, 3), Seq(3, 4)))
    assert(spiltSubArray(data, 3) === Seq(Seq(1, 2, 3), Seq(2, 3, 4)))
    assert(spiltSubArray(data, 4) === Seq(Seq(1, 2, 3, 4)))
  }
  test("heads 1") {
    val data = Seq(1, 2, 3, 4)
    assert(heads(data, 0) === Seq())
    assert(heads(data, 1) === Seq(Seq(1)))
    assert(heads(data, 2) === Seq(Seq(1), Seq(1, 2)))
    assert(heads(data, 3) === Seq(Seq(1), Seq(1, 2), Seq(1, 2, 3)))
    assert(heads(data, 4) === Seq(Seq(1), Seq(1, 2), Seq(1, 2, 3), Seq(1, 2, 3, 4)))
  }
  test("max 1") {
    val data = Seq(2, 4, -10, 2, -2)
    val k = 3
    assert(max(heads(data, k)) === MaxValue(6, 2))
  }
  test("solution 1") {
    assert(
      solution(Seq(2, 4, -10, 2, -2), 3) === Seq(6, 2)
    )
  }

  test("solution 2") {
    assert(
      solution(Seq(-2, 5, -1, -8), 2) === Seq(5)
    )
  }
}
