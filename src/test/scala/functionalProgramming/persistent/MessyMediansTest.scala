package functionalProgramming.persistent

import org.scalatest.{FunSuite, WordSpecLike}

/**
  * Created by yujieshui on 2017/1/16.
  */
class MessyMediansTest extends WordSpecLike {

  import MessyMedians._

  "test" must {
    "1" in {
      val in = Seq(1, 5, -2, 3, 2, 5, 4, -7, 2, - 3)
      val except = Seq(1, 1, 1, 1, 2, 2, 3, 1, 1, 3)
      val result = solution(in)

      assert(except === result)
    }
  }
}
