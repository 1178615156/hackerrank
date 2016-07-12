package algorithms.bitManipulation

import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/6/25.
  */
class XorSequenceTest extends FunSuite {

  import XorSequence._

  test("xor") {
    assert(
      (0 xor 1) === 1
    )
    assert(
      (1 xor 2) === 3
    )
    assert(
      (3 xor 3) === 0
    )
  }
  test("array") {
    assert(
      defaultXorHeadArray.head === 0
    )
    assert(
      defaultXorHeadArray.take(11).toList === List(
        0, 1, 3, 0, 4, 1, 7, 0, 8, 1, 11
      )
    )
    assert(defaultXorHeadArray(0)===0)
    assert(defaultXorHeadArray(4)===4)
    assert(defaultXorHeadArray(8)===8)
    assert(defaultXorHeadArray(16)===16)
    assert(defaultXorHeadArray(32)===32)
    assert(defaultXorHeadArray(64)===64)
    assert(defaultXorHeadArray(128)===128)
    assert(defaultXorHeadArray(512)===512)
  }
  test("xxx"){
    val x = defaultXorHeadArray.take(20).toList
    assert(defaultXorHeadArray(17) === xor(16,17))
  }




  test("xorStartNum"){
   assert( xorStartNum(0) ===0)
   assert( xorStartNum(1) ===1)
   assert( xorStartNum(2) ===3)
   assert( xorStartNum(3) ===0)
   assert( xorStartNum(4) ===4)
   assert( xorStartNum(5) ===1)
   assert( xorStartNum(4) ===4)
    assert((0l to 10L map xorStartNum).toList === List(
      0, 1, 3, 0, 4, 1, 7, 0, 8, 1, 11
    ))

  }
  test("array 2") {
  defaultXorHeadArray.take(100000).toSeq
  }
  test("compute") {
    println(defaultXorHeadArray.slice(2, 5).toList)
    println(sliceOf(2,5))
    assert(compute(2, 4) === 7)
    assert(compute(2, 8) === 9)
    assert(compute(5, 9) === 15)
  }

  test("compute 2"){
    println(
      compute(5,100000000)

    )
  }

}
