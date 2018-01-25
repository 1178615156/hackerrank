package algorithms.greedy

import org.scalatest.WordSpec
import PryankaAndToys._

class PryankaAndToysTest extends WordSpec {


  "solution" must {
    "1 2 3 17 10" in {
      val result = solution("1 2 3 17 10".split(" ").toList.map(_.toInt))
      assert(result === 3 )
    }
  }
}
