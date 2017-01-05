package functionalProgramming.ad_hoc

import org.scalatest.{FunSuite, WordSpecLike}

/**
  * Created by yujieshui on 2017/1/5.
  */
class MangoesTest extends WordSpecLike {

  import Mangoes._

  "solution" must {

    "0" in {
      val l = solution(
        5, 200,
        Seq(2, 5, 3, 2, 4),
        Seq(30, 40, 10, 20, 30)
      )
      assert(l == 3)
    }
    "1" in {
      val l = solution(
        2, 200,
        Seq(3, 4),
        Seq(1, 2)
      )
      assert(l == 2)
    }

    "all is 1" in {
      val l = solution(
        1,1,Seq(1),Seq(1)
      )
      assert(l ==1 )
    }
    "invited 0 friend" in {
      val l = solution(
        1,1,Seq(11),Seq(11)
      )
      assert(l ==0 )
    }
  }
}
