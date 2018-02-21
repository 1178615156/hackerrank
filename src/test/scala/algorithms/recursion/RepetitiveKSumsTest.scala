package algorithms.recursion

import org.scalatest._
import RepetitiveKSums._

class RepetitiveKSumsTest extends WordSpec with Matchers {
  ("can div") in {
    val result = canDivide(2, Seq(1, 4, 6, 8, 9))
    val expect = Seq(2, 3, 4)
    result shouldBe expect
  }
}
