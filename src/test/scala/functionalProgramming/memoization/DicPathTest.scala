package functionalProgramming.memoization

import org.scalatest.{FunSuite, WordSpecLike}
import DicPath._

/**
  * Created by yujieshui on 2017/2/17.
  */
class DicPathTest extends WordSpecLike {
  "solution" must {
    "case 1,2" in {
      assert(solution(1, 2) == 4)
      assert(solution(2, 1) == 6)
    }
    "case 2,2" in {
      assert(solution(2, 2) == 9)

    }
    "case 3 3" in {
      val result = solution(3, 3)
      println(111)
      assert(result == 19)
    }
    "case 2 2" in {
      val result = solution(2, 2)
      assert(result == 9)
    }
    "case 6 6" in {
      val result = solution(6, 6)
      println(result)
    }
    "case 10,10" in {
      val result = solution(10, 10)
      println(result)
    }
    "case 18,18" in {
      val result = solution(18, 18)
      println(result)
    }
  }
}
