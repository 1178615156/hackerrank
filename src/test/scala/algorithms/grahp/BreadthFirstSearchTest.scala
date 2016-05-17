package algorithms.grahp

import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/5/16.
  */
class BreadthFirstSearchTest extends FunSuite {

  import BreadthFirstSearch._

  test("one") {
    val start = 1
    val list = List(
      Edge(1, 2),
      Edge(1, 3)
    )
    assert(solution(start, list,1 to 4) == Seq(6, 6, -1))
  }

  test("two"){
    val start =2
    val list = List(
      Edge(2,3)
    )
    assert(solution(start,list,1 to 3) == Seq(-1,6))
  }
}
