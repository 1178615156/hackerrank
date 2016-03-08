package algorithms.implementation

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/3/8.
  */
class MatrixRotation$Test extends FunSuite {

  import MatrixRotation._

  test("rotation line ") {
    val l = List(1, 2, 3, 4)
    assert(rotationLine(0, l) == l)
    assert(rotationLine(1, l) == List(2, 3, 4, 1))
    assert(rotationLine(2, l) == List(3, 4, 1, 2))
    assert(rotationLine(3, l) == List(4, 1, 2, 3))
    assert(rotationLine(4, l) == List(1, 2, 3, 4))
    assert(rotationLine(5, l) == List(2, 3, 4, 1))
  }


  test("sub matrix ") {
    assert(subMatrix(List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)
    )) == List(List(5)))
    assert(subMatrix(List(

    )) == Nil)

    assert(subMatrix(List(
      List(1)
    )) == EmptyMatrix)

    assert(subMatrix(List(
      List(1),
      List(2)
    )) == EmptyMatrix)
    assert(subMatrix(List(
      List(1, 0),
      List(2, 0)
    )) == EmptyMatrix)
    assert(subMatrix(List(
      List(1),
      List(2),
      List(3)
    )) == List(List()))
    assert(subMatrix(List(
      List(1, 0),
      List(2, 0),
      List(3, 0)
    )) == List(List()))
  }

  test("line ") {
    assert(line(List(
      List(1))
    ) == List(1))

    assert(line(List(
      List(1, 2, 3)
    )) == List(1, 2, 3))

    assert(line(List(
      List(1, 2),
      List(3, 4)
    )) == List(1, 2, 4, 3))
    assert(line(List(
      List(1, 2),
      List(3, 4),
      List(5, 6)
    )) == List(1, 2, 4, 6, 5, 3))

    assert(line(List(
      List(1, 2, 3, 4),
      List(7, 8, 9, 10),
      List(13, 14, 15, 16),
      List(19, 20, 21, 22),
      List(25, 26, 27, 28)
    )) == List(1, 2, 3, 4, 10, 16, 22, 28, 27, 26, 25, 19, 13, 7))
  }
}
