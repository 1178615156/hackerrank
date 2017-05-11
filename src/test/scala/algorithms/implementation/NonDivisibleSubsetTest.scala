package algorithms.implementation

import org.scalatest.FunSuite
import NonDivisibleSubset._

/**
  * Created by yujieshui on 2017/5/10.
  */
class NonDivisibleSubsetTest extends FunSuite {
  test("sum divisible") {
    println(sumDivisible(5))
  }
  test("impl") {
    println(impl(Seq(0, 1, 2, 3, 4), sumDivisible(5).toSet, Seq(Set())).mkString("\n"))
  }
  test("impl 2") {
    println(impl(0 to 9, sumDivisible(10).toSet, Seq(Set())).mkString("\n"))

  }
  test("solution") {
    println(solution(3, "1 7 2 4".split(" ").map(_.toInt)))
  }
}
