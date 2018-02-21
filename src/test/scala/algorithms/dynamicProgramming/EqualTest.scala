package algorithms.dynamicProgramming

import org.scalatest._
import Equal._

class EqualTest extends WordSpec with Matchers {
  def string2listint(s: String) =
    s.split(" ").map(_.toLong).toList

  "2 2 3 7" in {
    val result = solution("2 2 3 7".split(" ").map(_.toLong).toList)
    val expect = 2
    result shouldBe expect
  }
  "1 5 5" in {
    (solution(string2listint("1 5 5"))) shouldBe 3
  }
  "2 5 5 5 5 5" in {
    val result = solution(string2listint("2 5 5 5 5 5"))
    (result) shouldBe 6
  }

  "51" in {
    val data = string2listint("512 125 928 381 890 90 512 789 469 473 908 990 195 763 102 643 458 366 684 857 126 534 974 875 459 892 686 373 127 297 576 991 774 856 372 664 946 237 806 767 62 714 758 258 477 860 253 287 579 289 496")
    val result = solution(data)
    val expect = 5104
    result should be(expect)
  }
}
