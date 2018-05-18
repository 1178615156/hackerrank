package algorithms.implementation

import org.scalatest.FunSuite

class SequenceEquationTest extends FunSuite {
  import SequenceEquation._
  test("2,3,1"){
    val result = solution(List(2,3,1)).toList
    assert(result === List(2,3,1))
    println(result)
  }
}
