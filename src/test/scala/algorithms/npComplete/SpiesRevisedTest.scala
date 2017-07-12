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
    val n = 21
    val result = solution(empty, 1, n)
    println(result)
    println(solutionToString(result.get, n))
  }

  import org.scalameter._

  "mersure" in {
    val n = 23
   val time = config().withWarmer(Warmer.Zero)//Key.exec.minWarmupRuns -> 10, Key.exec.maxWarmupRuns -> 10)
      .measure(
        solution(empty, 1, n)
      )
    println(time )

  }
  "some line" in {
    //    assert(someLine(Point(4, 9), Point(3, 6), Point(2, 3)))
    //    assert(someLine(Point(4, 9), Point(2, 3), Point(3, 6)))
    //
    //    assert(someLine(Point(2, 3), Point(4, 9), Point(3, 6)))
    //    assert(someLine(Point(2, 3), Point(3, 6), Point(4, 9)))
    //
    //    assert(someLine(Point(2, 3), Point(4, 9), Point(3, 6)))
    //    assert(someLine(Point(2, 3), Point(3, 6), Point(4, 9)))
    //
    //    assert(someLine(Point(0, 0), Nil) === false)
    //    assert(someLine(Point(0, 0), Seq(Point(1, 1))) === false)
    assert(someLine(Point(2, 2), Seq(Point(1, 1), Point(7, 8))) === false)
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
