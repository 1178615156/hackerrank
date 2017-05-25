package algorithms.search

import org.scalatest.FunSuite
import AbsoluteElementSums._

import scala.util.Random

/**
  * Created by yujieshui on 2017/5/25.
  */
class AbsoluteElementSumsTest extends FunSuite {
  test("solution") {
    val elements = "-1 2 -3".split(" ").map(_.toLong).toList
    val queries = "1 -2 3".split(" ").map(_.toLong).toList

    val result = solution(elements, queries)
    println(result.toList)
  }
  test("m"){
    val start = System.currentTimeMillis()
    val ramdon= new Random()
    val element = 1 to 500000 map (_ =>ramdon.nextInt(4000).toLong - 2000)
    val query = 1 to 500000 map (_ => ramdon.nextInt(4000).toLong - 2000)
    val result = solution(element,query)
    val end = System.currentTimeMillis()
    println(end - start )
  }
}
