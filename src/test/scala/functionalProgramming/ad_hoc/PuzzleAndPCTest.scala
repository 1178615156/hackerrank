package functionalProgramming.ad_hoc

import org.scalatest.FunSuite

import PuzzleAndPC._

class PuzzleAndPCTest extends FunSuite {

  test("1, 2 2") {
    assert(solution(1, Point(2, 2)) === List(Tromino(Point(1, 1), Point(1, 2), Point(2, 1))))
  }

  test("2, 1 1") {
    val result = solution(2, Point(1, 1))
    println(result.mkString("\n"))
  }
  test("2, 2 2") {
    val result = solution(2, Point(2, 2))
    println(debug(2, result))
  }
  test("2, 4 4") {
    val result = solution(2, Point(4, 4))
    println(debug(2, result))
  }
  test("2"){
    for {
      x <- 1 to 4
      y <- 1 to 4
    }yield {
      val result = solution(2, Point(x,y))
      println(s"""---$x $y---""")
      println(debug(2, result))

    }
  }

  def debug(n: Int, seq: Seq[Tromino]) = {
    val array: Array[Array[Int]] = Array.fill(2 << (n - 1))(Array.fill(2 << (n - 1))(0))
    seq.zipWithIndex.foreach { case (tromino, i) =>
      array(tromino.a.x - 1)(tromino.a.y - 1) = i + 1
      array(tromino.b.x - 1)(tromino.b.y - 1) = i + 1
      array(tromino.c.x - 1)(tromino.c.y - 1) = i + 1
    }
    array.map(_.toSeq.mkString(" ")).mkString("\n")
  }
}
