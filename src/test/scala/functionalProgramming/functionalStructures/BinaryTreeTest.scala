package functionalProgramming.functionalStructures

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/8/19.
  */
class BinaryTreeTest extends FunSuite {

  import BinaryTree._

  test("apply empty") {
    val array = Seq[Int]()
    val tree = apply(array, 0, 0)
    assert(tree.isEmpty)
  }
  test("apply Seq(1)") {
    val array = Seq(1)
    val tree = apply(array, 0, array.size)
    assert(tree.min === 1)
    assert(tree.start === 0)
    assert(tree.end === 1)


  }
  test("apply Seq(1, 2, 3, 4, 5)") {
    val array = Seq(1, 2, 3, 4, 5)
    val tree = apply(array, 0, array.size)
    assert(tree.min === 1)
    tree match {
      case Binary(left, right, start, end) =>
        assert(left.min === 1)
        assert(left.start === 0)
        assert(left.end === 2)

        assert(right.min === 3)
        assert(right.start === 2)
        assert(right.end === 5)
    }
  }
}
