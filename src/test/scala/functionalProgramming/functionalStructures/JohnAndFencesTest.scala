package functionalProgramming.functionalStructures

import org.scalatest.{FunSuite, WordSpecLike}

/**
  * Created by yujieshui on 2017/1/31.
  */
class JohnAndFencesTest extends WordSpecLike {

  import JohnAndFences._

  "carver_opt" must {
    "case 1" in {
      val result = carver_opt(Seq(2, 5, 7, 4, 1, 8),0)
      println(result)
    }
  }
  "solution" must {
    "case 1 " in {
      val result = solution(Seq(2, 5, 7, 4, 1, 8))
      assert(result == 12)
    }
  }
}
