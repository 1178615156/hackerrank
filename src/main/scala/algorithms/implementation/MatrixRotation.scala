package algorithms.implementation

import utils.SetInt

/**
  * Created by yujieshui on 2016/3/7.
  */

object MatrixRotation {

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  type Matrix = Seq[Seq[Int]]
  type MatrixList = Seq[Int]
  type SubMatrix = Matrix
  val EmptyMatrixList: MatrixList = Nil
  val EmptyMatrix    : Matrix     = Nil


  def rotationLine(rotationNum: Int, rt: Seq[Int]): Seq[Int] = {
    if (rotationNum == 0 || rt.isEmpty)
      rt
    else {
      val takeNum = rotationNum % rt.size
      rt.drop(takeNum) ++ rt.take(takeNum)
    }
  }

  def line(matrix: Matrix): Seq[Int] = {
    if (matrix.isEmpty)
      Nil
    else if (matrix.size == 1)
      matrix.head
    else if (matrix.head.size == 1)
      matrix.map(_.head)
    else {
      val head = matrix.head
      val last = matrix.last.reverse
      val right = matrix.drop(1).dropRight(1).map(_.last)
      val left = matrix.drop(1).dropRight(1).map(_.head).reverse
      head ++ right ++ last ++ left
    }
  }


  def subMatrix(matrix: Matrix) = {
    if (matrix.size < 2)
      EmptyMatrix
    else {
      matrix.drop(1).dropRight(1).map(_.drop(1).dropRight(1))
    }
  }

  def rotation(matrix: Matrix, rotationNum: Int, matrix_rowNum: Int, matrix_columnNum: Int): Matrix = {
    if (matrix_rowNum <= 0 || matrix_columnNum <= 0) {
      matrix
    } else {
      var matrix_line = rotationLine(rotationNum,line(matrix))
      val sub_matrix = subMatrix(matrix)
      val sub_matrix_rotation = rotation(sub_matrix, rotationNum, matrix_rowNum - 2, matrix_columnNum - 2)

      val new_head = matrix_line.take(matrix_columnNum)
      matrix_line = matrix_line.drop(matrix_columnNum)

      val new_right: Seq[Int] = matrix_line.take(matrix_rowNum - 2)
      matrix_line = matrix_line.drop(matrix_rowNum - 2)

      val new_last = matrix_line.take(matrix_columnNum).reverse
      matrix_line = matrix_line.drop(matrix_columnNum)

      val new_left: Seq[Int] = matrix_line.reverse


      val t =
        sub_matrix_rotation.map(Option.apply)
          .zipAll(new_left.map(Option.apply), None, None)
          .zipAll(new_right.map(Option.apply), (None, None), None)
      val new_sub_matrix_rotation: Seq[Seq[Int]] =
        t map {
          case ((line: Option[Seq[Int]], left: Option[Int]), right: Option[Int]) =>
            left +: line.getOrElse(Nil).map(Option.apply) :+ right collect { case Some(x) => x }
        }
      new_head +: new_sub_matrix_rotation :+ new_last
    }
  }

  def main(args: Array[String]) {
    val n :: m :: rotationNum :: Nil = readListInt()
    val inData = 1 to n map (_ => readListInt())
    val result = rotation(inData, rotationNum, n, m)
    println(result.map(_.mkString(" ")).mkString("\n"))


  }

  SetInt(
    """5 4 7
      |1 2 3 4
      |7 8 9 10
      |13 14 15 16
      |19 20 21 22
      |25 26 27 28
    """.stripMargin)

  //  SetInt(
  //    """10 8 40
  //      |9718805 60013003 5103628 85388216 21884498 38021292 73470430 31785927
  //      |69999937 71783860 10329789 96382322 71055337 30247265 96087879 93754371
  //      |79943507 75398396 38446081 34699742 1408833 51189 17741775 53195748
  //      |79354991 26629304 86523163 67042516 54688734 54630910 6967117 90198864
  //      |84146680 27762534 6331115 5932542 29446517 15654690 92837327 91644840
  //      |58623600 69622764 2218936 58592832 49558405 17112485 38615864 32720798
  //      |49469904 5270000 32589026 56425665 23544383 90502426 63729346 35319547
  //      |20888810 97945481 85669747 88915819 96642353 42430633 47265349 89653362
  //      |55349226 10844931 25289229 90786953 22590518 54702481 71197978 50410021
  //      |9392211 31297360 27353496 56239301 7071172 61983443 86544343 43779176
  //    """.stripMargin)
}
