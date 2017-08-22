package algorithms.recursion

import algorithms.recursion.PasswordCracker.solution
import org.scalatest.{FunSuite, WordSpec}

class PasswordCrackerTest extends WordSpec {


  "because can do must we what" in {
    val result = solution(
      "because can do must we what".split(" ").toList,
      "wedowhatwemustbecausewecan"
    )
    assert(result.get.mkString(" ") === "we do what we must because we can")
  }

}
