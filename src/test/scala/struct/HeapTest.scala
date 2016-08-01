package struct

import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/8/1.
  */
class HeapTest extends FunSuite {

  import struct.Heap._

  val heap = apply(Seq(1, 2, 3))

  test("apply") {
    println(heap)
    assert(heap.max === 3)
    assert(dropMax(heap).max === 2)
    println(dropMax(heap))
    assert(dropMax(dropMax(heap)).max === 1)
  }

  test("insert"){
    println(insert(4, heap))
    assert(insert(4, heap).max === 4)
    assert(insert(4,insert(5,heap)).max === 5)
    assert(dropMax(insert(heap.max,heap)).max === heap.max)
  }


  test("merge"){
    val heap2= apply(Seq(4,5,6))
    val _heap = merge(heap,heap2)
    println(_heap)
    assert(_heap.max === 6)
    assert(dropMax(_heap).max === 5)
    assert(dropMax(dropMax(_heap)).max ===4 )
    assert(dropMax(dropMax(dropMax(_heap))).max === 3)
  }
}
