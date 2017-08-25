package algorithms.recursion

import org.scalatest.{FunSuite, WordSpec}
import ThePowerSum._

class ThePowerSumTest extends WordSpec {

  "solution" must {
    "10 2" in {
      println(solution(10, 2))
    }
    "100 2" in {
      val result = solution(100, 2)
      println(result)
    }
  }
}
