package algorithms.gameTheory

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/10/5.
  */
class FunGameTest extends FunSuite {

  import FunGame._
  import Play._

  var i = 0

  def get_index = {
    i += 1
    i
  }

  implicit def set2seq[T](set: Set[T]) = set.toSeq

  test("impl") {
    println(impl(First, Result(0, 0), Set(
      Entity(1, 5, get_index),
      Entity(3, 3, get_index),
      Entity(4, 1, get_index)
    )))
  }
  test("impl 0") {
    assert(impl(First, Result(0, 0), Seq(
      Entity(4, 1, get_index)
    )) === Seq(Result(4, 0)))
  }
  test("impl 1") {
    println(impl(First, Result(0, 0), Set(
      Entity(4, 1, get_index),
      Entity(4, 2, get_index),
      Entity(4, 3, get_index)

    )))
  }
  test("sample 1") {
    assert(
      solution(Set(
        Entity(1, 5, get_index),
        Entity(3, 3, get_index),
        Entity(4, 1, get_index)
      )) === Play.First
    )
    assert(solution(Set(
      Entity(1, 1, get_index),
      Entity(1, 1, get_index)
    )) === Tie
    )
    assert(solution(Set(
      Entity(2, 3, get_index),
      Entity(2, 3, get_index)
    )) === Second
    )
    println(
      solution(Set(
        Entity(2, 3, get_index),
        Entity(2, 3, get_index)
      ))
    )
  }
  test("sample 2"){
    assert(solution(Seq(
      Entity(1, 1, get_index),
      Entity(1, 1, get_index)
    )) === Tie
    )
  }
}
