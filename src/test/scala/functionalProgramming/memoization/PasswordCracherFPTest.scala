package functionalProgramming.memoization

import org.scalatest.{FunSuite, WordSpecLike}
import PasswordCracherFP._

/**
  * Created by yujieshui on 2017/3/7.
  */
class PasswordCracherFPTest extends WordSpecLike {
  "solution" must {

    "case i" in {
      val words = "because can do must we what".split(" ").toList
      val string = "wedowhatwemustbecausewecan"
      val result = solution(words, string)
      val expect = "we do what we must because we can"
      assert(result === expect)
    }
    "case 2" in {
      val words = "hello planet".split(" ").toList
      val string = "helloworld"
      val result = solution(words, string)
      println(result)
    }
    "case 3" in {
      val words = "ab abcd cd".split(" ").toList
      val string = "abcd"
      val result = solution(words, string)
      println(result)
    }

  }
}
