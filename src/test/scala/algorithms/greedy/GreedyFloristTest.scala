package algorithms.greedy

import org.scalatest.FunSuite
import GreedyFlorist._

/**
  * Created by yujieshui on 2017/5/12.
  */
class GreedyFloristTest extends FunSuite {
  test("0") {
    println(solution(Seq(2, 5, 6), 3))
  }
  test("1"){
    println(solution(Seq(2, 5, 6), 2))
  }
}
