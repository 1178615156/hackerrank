package algorithms.implementation

/**
  * Created by yujieshui on 2017/5/23.
  */
object OrganizingContainersOfBalls {
  type Contain = Vector[Long]
  type Matrix = Seq[Contain]

  def move(contain: Contain, matrix: Matrix, close: Matrix): Option[Matrix] = {

    val indexSwap =
      contain.tail.find(_ > 0).map(contain.tail.indexOf).map(_ + 1).filter(_ > 0)

    if(indexSwap.isEmpty && matrix.isEmpty) Some(close.map(_.tail))
    else if(indexSwap.isEmpty || matrix.isEmpty) None
    else {
      val row = matrix.head
      val index = indexSwap.get
      val containHead = contain.head
      val rowHead = row.head
      val containVal = contain(index)
      val rowVal = row(index)
      if(containVal >= rowHead) {
        val newContain = contain
          .updated(0, containHead + rowHead)
          .updated(index, containVal - rowHead)

        val newRow = row
          .updated(0, 0L)
          .updated(index, row(index) + rowHead)
        move(newContain, matrix.tail, close :+ newRow)
      }
      else {
        val newContain = contain
          .updated(0, containHead + containVal)
          .updated(index, 0L)

        val newRow = row
          .updated(0, row.head - containVal)
          .updated(index, row(index) + containVal)
        move(newContain, newRow +: matrix.tail, close)
      }

    }
  }

  def split(head: Matrix, now: Contain, tail: Matrix): Seq[(Contain, Matrix)] = {
    if(tail.isEmpty)
      Seq(now -> head)
    else
      (now -> (head ++ tail)) +: split(head :+ now, tail.head, tail.tail)
  }

  def empty: Matrix = Seq()

  def solution(matrix: Matrix): Boolean = {
    if(matrix.isEmpty) true
    else {
      val next = split(empty, matrix.head, matrix.tail).toStream
        .map { case (contain, matrix) => move(contain, matrix, empty) }
        .collect { case Some(x) => x }
      if(next.isEmpty) false
      else next.exists(solution)
    }

  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val q :: Nil = readListInt()
    val data =
      1 to q map { _ =>
        val n :: Nil = readListInt()
        1 to n map (_ => io.StdIn.readLine().split(" ").toVector.map(_.toLong))
      }

    val result = data.map(solution).map {
      case true  => "Possible"
      case false => "Impossible"
    }
    println(result.mkString("\n"))
  }
}
