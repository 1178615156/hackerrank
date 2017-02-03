package functionalProgramming.memoization

import org.scalatest.{FunSuite, WordSpecLike}

/**
  * Created by yujieshui on 2017/2/2.
  */
class ReverseFactorizationTest extends WordSpecLike {

  import ReverseFactorization._

  "impl opt" must {
    "case 1 " in {
      val result = impl_opt(Seq(12 -> Seq()), Seq(2, 3, 4), Set(12))
      println(result)
    }
    "case 2 " in {
      val result = impl_opt(Seq(15 -> Seq()), Seq(2, 10, 6, 9, 11), Set(15))
      println(result)
    }
    "case 3" in {
      val result = impl_opt(Seq(72 -> Seq()), "2 4 6 9 3 7 16 10 5".split(" ").map(_.toInt), Set(72))
      println(result)
    }
    "1000000000" in {
      val result = impl_opt(Seq(1000000000 -> Seq()), "2 4 5 7".split(" ").map(_.toInt), Set(1000000000))
      println(result)
    }
    "2 * 2 * 2 * 2 * 2 * 3 * 3 * 3 * 5 * 5 * 5 * 7 * 7 * 7" in {
      val n = 2 * 2 * 2 * 2 * 2 * 3 * 3 * 3 * 5 * 5 * 5 * 7 * 7 * 7 * 7
      val result = impl_opt(Seq(n -> Seq()), "2 3 4 5 7".split(" ").map(_.toInt), Set(n))
      println(result)
    }
  }
  "solution" must {
    "case 1 " in {
      assert(solution(12, Seq(2, 3, 4)) == Seq(1, 3, 12))
    }
    "case 2" in {
      assert(solution(15, Seq(2, 10, 6, 9, 11)) == Seq(-1))
    }
    "case 3" in {
      assert(solution(72, "2 4 6 9 3 7 16 10 5".split(" ").map(_.toInt)) == Seq(1, 2, 8, 72))
    }
  }
}
