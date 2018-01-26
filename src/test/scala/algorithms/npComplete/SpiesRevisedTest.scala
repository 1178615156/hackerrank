package algorithms.npComplete

import org.scalatest.{FunSuite, WordSpec}

import scala.collection.immutable.TreeSet

/**
  * Created by yujieshui on 2017/7/12.
  */
class SpiesRevisedTest extends WordSpec {

  import SpiesRevised._

  "solution 7" in {
    val n = 7
    val result = solution(empty, 1, 7)
    println(result)
    println(solutionToString(result.get, 7))
  }
  "solution 11" in {
    val n = 11
    val result = solution(empty, 1, n)
    println(result)
    println(solutionToString(result.get, n))
  }
  "solution 13" in {
    val n = 13
    val result = solution(empty, 1, n)
    println(result)
    println(solutionToString(result.get, n))
  }
  "solution n" in {
    val n = 27
    val result = solution(empty, 1, n)
    checkResults(result.get)
    println(result.get.reverse)
    println(solutionToString(result.get, n))
  }

  //  import org.scalameter._
  //
  //  "mersure" in {
  //    val n = 23
  //    val time = config().withWarmer(Warmer.Zero) //Key.exec.minWarmupRuns -> 10, Key.exec.maxWarmupRuns -> 10)
  //      .measure(
  //      solution(empty, 1, n)
  //    )
  //    println(time)
  //
  //  }
  "cover" must {

  }
  "some line" must {
    "0" in {
      assert(sameLine(Point(2, 2), List(Point(1, 1), Point(7, 8))) === false)

    }
    "a" in {

      assert(sameLine(Point(4, 9), Point(3, 6), Point(2, 3)))
      assert(sameLine(Point(4, 9), Point(2, 3), Point(3, 6)))

      assert(sameLine(Point(2, 3), Point(4, 9), Point(3, 6)))
      assert(sameLine(Point(2, 3), Point(3, 6), Point(4, 9)))

      assert(sameLine(Point(2, 3), Point(4, 9), Point(3, 6)))
      assert(sameLine(Point(2, 3), Point(3, 6), Point(4, 9)))

      assert(sameLine(Point(0, 0), Nil) === false)
      assert(cover(Point(0, 0), Point(1, 1)) === false)
      assert(sameLine(Point(0, 0), List(Point(1, 1))) === false)
      assert(sameLine(Point(2, 2), List(Point(1, 1), Point(7, 8))) === false)
      assert(sameLine(Point(3, 5), Point(1, 1), Point(2, 3)) === true)
      assert(sameLine(Point(1, 1), Point(2, 3), Point(3, 5)) === true)
    }
    "b" in {
      assert(sameLine(Point(3, 5), List(Point(1, 1), Point(2, 3))) === true)
      assert(sameLine(Point(5, 7), List(Point(1, 1), Point(3, 4))) === true)
      assert(sameLine(Point(17, 25), List(Point(1, 1), Point(5, 7))) === true)
    }
    "c" in {
      val r = "1 3 6 2 7 10 23 26 18 15 24 19 9 25 21 8 12 5 27 13 20 4 17 14 22 11 16".split(" ").map(_.toInt).toList
      val len = r.length
      val points = 1 to r.length zip r map { case (i, j) => Point(i, j) }
      for {
        a <- points
        b <- points
        c <- points if a != b && a != c && b != c
      } {
        assert(sameLine(a, List(b, c)) === false, (a, b, c))
      }

    }

//    "d" in {
//      val r = "1 3 6 2 7 5 11 13 16 20 17 24 26 23 4 27 25 9 12 14 8 10 18 21 15 22 19".split(" ").toList.map(_.toInt)
//      val len = r.length
//      val points = 1 to r.length zip r map { case (i, j) => Point(i, j) }
//      checkResults(points)
//
//    }
  }

  def checkResults(points: Seq[Point]) = {
    for {
      a <- points
      b <- points
      c <- points if a != b && a != c && b != c
    } {
      assert(sameLine(a, List(b, c)) === false, (a, b, c))
    }
  }

  def solutionToString(_seq: Iterable[Point], n: Int) = {
    val seq = _seq.toSeq
    1 to n map { x =>
      1 to n map { y =>
        if(seq.contains(Point(x, y))) "S"
        else "*"
      } mkString (" ")
    } mkString ("\n")
  }
}
