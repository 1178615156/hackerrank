package algorithms.dynamicProgramming

import org.scalatest.{FunSuite, WordSpec}
import TheCoinChangeProblem._

class TheCoinChangeProblemTest extends WordSpec {
  "4 -> 1 2 3" in {
    println(solution(4, Seq(1, 2, 3)))
  }

  "10 -> 2 5 3 6" in {
    println(solution(10, Seq(2, 5, 3, 6)))
    println(cache.mkString("\n"))
  }

  "impl" in {
    //    println(impl2(10, Seq(2, 5, 3, 6)))
    //    println(impl(4, Seq(1, 2, 3)))
    //    println(impl2(2,Seq(1,2)))

//    println(impl(5, Seq(2, 3)))
    println(solution(5, Seq(2, 5, 3, 6)))
    println(cache.mkString("\n"))
  }
}
