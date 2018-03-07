package dataStructures.arrays

import org.scalatest.FunSuite

class AlgorithmicCrushTest extends FunSuite {
  test("solution") {
    import AlgorithmicCrush._
    val result = solution(5,Seq(
      Action(1,2,100),
      Action(2,5,100),
      Action(3,4,100)
    ))
    val expect = 200L
    assert(result == expect)

  }
}
