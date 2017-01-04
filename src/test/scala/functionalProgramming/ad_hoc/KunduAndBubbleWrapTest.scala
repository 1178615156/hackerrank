package functionalProgramming.ad_hoc

import org.scalatest.{FunSuite, WordSpecLike}

/**
  * Created by yujieshui on 2017/1/2.
  */
class KunduAndBubbleWrapTest extends WordSpecLike {
  import KunduAndBubbleWrap._
    "solution" must {
      "1 1" in {
        assert(solution(1, 1) === 1)
      }
      "1 2 " in {
        assert(solution(1, 2) === 3)
      }
      "2 2 " in {
        assert(solution(2,2)  / 8.333 )
      }
    }
}
