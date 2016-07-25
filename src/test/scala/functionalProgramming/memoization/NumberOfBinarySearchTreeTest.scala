package functionalProgramming.memoization

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/7/25.
  */
class NumberOfBinarySearchTreeTest extends FunSuite {

  import NumberOfBinarySearchTree._


  val cacheMap_ = cacheMap(1000)
  test("y") {
    assert(
      solution(1,cacheMap_) === 1
    )
    assert(
      solution(2,cacheMap_) === 2
    )
    assert(
      solution(3,cacheMap_) === 5
    )
    assert(
      solution(4,cacheMap_) === 14
    )
  }
  test("100") {
    import org.scalameter._
    val time = config(Key.exec.benchRuns -> 1) withWarmer Warmer.Zero measure {
      assert( solution(100,cacheMap_) === 25666077)
    }
    println(
      time
    )
  }

  test("1000") {
    import org.scalameter._
    val time = config(Key.exec.benchRuns -> 1) withWarmer Warmer.Zero measure {
      ( solution(1000,cacheMap_))
    }
    println(
      time
    )
  }
}
