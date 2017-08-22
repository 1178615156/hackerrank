package algorithms.recursion

import org.scalatest.{FunSuite, WordSpec}
import ArithmeticExpressions._

class ArithmeticExpressionsTest extends WordSpec {

  "22 79 21" in {
    val result = solution("22 79 21".split(" ").toList.map(_.toInt))
    assert(
      result.get === "22*79-21" ||
        result.get === "22+79*21")
  }
}
