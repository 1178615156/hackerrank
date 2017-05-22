package algorithms.search

import org.scalatest.FunSuite
import HacherlandRadioTransmitters._

/**
  * Created by yujieshui on 2017/5/22.
  */
class HacherlandRadioTransmittersTest extends FunSuite {

  test("1 2 3 4 5") {
    println(solution("1 2 3 4 5".split(" ").map(_.toInt), 1))
  }
  test("7 2 4 6 5 9 12 11 ") {
    println(solution("7 2 4 6 5 9 12 11 ".split(" ").map(_.toInt), 2))
  }

  test("oldGreedy"){
    val data = "9 5 4 2 6 15 12".split(" ").toList.distinct.map(_.toInt).sorted
    println(solution(data,2 ))

//    println(solution(data,2))
  }
}
