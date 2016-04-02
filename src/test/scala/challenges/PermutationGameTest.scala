package challenges

import algorithms.greedy.PermutationGame
import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/2/20.
  */
class PermutationGameTest extends FunSuite {

  import PermutationGame._

  test("listIsSort") {
    assert(listIsSort(Nil))
    assert(listIsSort(1 to 1))
    assert(listIsSort(1 to 10))
    assert(!listIsSort(List(2, 1)))
    assert(!listIsSort((1 to 10).toList ++ List(1)))
  }

  implicit def list2vertor(l: List[Int]) = l.toVector

  test("makeTriangle") {
    println(
      makeTriangle(Nil)
    )
    println(
      makeTriangle(List(1))
    )
    println(
      makeTriangle(List(1, 2))
    )
    //    assert(makeTriangle(Nil) == Nil)
    //    assert(makeTriangle(List(1)) == List(
    //      List(1)
    //    ))
    //    assert(makeTriangle(List(1, 2)) == List(
    //      List(1),
    //      List(1, 2)
    //    ))
  }

  implicit class StringWithAsListInt(s: String) {
    def asListInt = s.split(" ").map(_.toInt).toList
  }

  test("game") {
    println(game(List(1, 3, 2)))
    println(game(List(5, 3, 2, 1, 4)))

  }

  test("game time ") {
    println(game(List(1, 3, 2)))
    val bt = System.currentTimeMillis()
    println(game("8 1 6 9 2 3 4 5 7".asListInt))
    println(game("5 1 11 3 2 4 12 6 7 10 8 9".asListInt))
    println(game("9 13 8 6 7 5 3 10 4 12 2 11 1".asListInt))
    println(game("6 4 1 5 3 2".asListInt))
    println(game("1 3 2 4 5".asListInt))
    println(game("6 3 5 4 1 2".asListInt))
    println(game("8 1 6 9 2 3 4 5 7".asListInt))
    println("time : " + (System.currentTimeMillis() - bt))
  }
}






















