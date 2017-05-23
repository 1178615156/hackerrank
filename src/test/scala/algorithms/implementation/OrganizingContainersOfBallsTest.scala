package algorithms.implementation

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2017/5/23.
  */
class OrganizingContainersOfBallsTest extends FunSuite {

  import OrganizingContainersOfBalls._

  test("split") {
    val matrix = Seq(
      Seq(1, 2, 3).map(_.toLong),
      Seq(4, 5, 6).map(_.toLong),
      Seq(7, 8, 9).map(_.toLong)
    )
    val result = split(Seq(), matrix.head, matrix.tail)
    println(result.mkString("\n"))
  }

  test("solution 0") {
    val matrix = Seq(
      Seq(1, 1),
      Seq(1, 1)
    ).map(_.map(_.toLong))
    val result = solution(matrix)
    assert(result)
  }

  test("solution 1") {
    val matrix = Seq(
      Seq(0, 2),
      Seq(1, 1)
    ).map(_.map(_.toLong))
    val result = solution(matrix)
    assert(result === false)
  }

}
