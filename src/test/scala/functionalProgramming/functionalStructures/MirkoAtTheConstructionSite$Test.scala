package functionalProgramming.functionalStructures

import org.scalatest.FunSuite

import scala.util.Random

/**
  * Created by yujieshui on 2016/8/21.
  */
class MirkoAtTheConstructionSite$Test extends FunSuite {

  import MirkoAtTheConstructionSite._

  val data1 = Seq(
    Line(7, 1, 1),
    Line(5, 2, 2),
    Line(1, 3, 3)
  )

  test("impl") {
    val result =
      impl(data1, startLine(data1), Point(0, 0))
    println(
      result.mkString("\n")
    )
  }
  test("solution2 test"){
    val data = data1
    val query = 0 to 5
    assert(
      solution(data, query) === solution2(data, query)
    )

  }
  test("solution2 1") {
    val data = Seq(
      Line(1, 1, 1),
      Line(1, 1, 2)
    )
    val result =
      impl(data, startLine(data), Point(0, 0))
    println(
      result.mkString("\n")
    )
  }

  test("random data") {
    val random = new Random(88)

    val data = 1 to 10 map { i => Line(random.nextInt(100), random.nextInt(100), i) }
    val query = 0 to 10000
    assert(
      solution(data, query) === solution2(data, query)
    )
  }
  test("random data 2") {
    val random = new Random(88)

    val data = 1 to 100 map { i => Line(random.nextInt(100), random.nextInt(100), i) }
    val query = 0 to 10000
    assert(
      solution(data, query) === solution2(data, query)
    )
  }

  test("random data 3") {
    val random = new Random(88)

    val data = 1 to 10 map { i => Line(random.nextInt(10), random.nextInt(10), i) }
    val query = 0 to 10
    println(solution(data, query))
    println(solution2(data, query))
    assert(
      solution(data, query) === solution2(data, query)
    )
  }
}
