package algorithms.greedy

import org.scalatest.{FunSuite, WordSpec}
import FightingPits._
import struct.Heap

/**
  * Created by yujieshui on 2017/7/13.
  */
class FightingPitsTest extends WordSpec {

  val x = Heap.apply(Seq(1, 1, 2))
  val y = Heap.apply(Seq(1, 1, 1, 2))

  "attack" must {

    "a" in {
      val result = attack(x, y)
      assert(result)
    }
    "b" in {
      assert(attack(y, x) === true)
    }
  }
  "solution" in {


    val query = List(
      Attack(1, 2),
      Attack(2, 1),
      Add(2, 1),
      Add(2, 1),
      Attack(1, 2),
      Attack(2, 1)
    )
    assert(solution(Map(1 -> x, 2 -> y), query) === Seq(1, 2, 1, 1))
  }
}
