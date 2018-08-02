package algorithms.strings

import org.scalatest.{FunSuite, WordSpec}
import BuildPalindrome._

class BuildPalindromeTest extends WordSpec {

  "bac bac" in {
    println(buildPalindrome("bac", "bac"))
  }
  "abc def" in {
    println(buildPalindrome("abc", "def"))
  }

  "jdfh fds" in {
    println(buildPalindrome("jdfh", "fds"))
  }
}
