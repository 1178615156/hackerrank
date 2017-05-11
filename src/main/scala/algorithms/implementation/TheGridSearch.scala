package algorithms.implementation

/**
  * Created by yuJieShui on 2016/4/24.
  */
object TheGridSearch {

  case class Grid(digits: Seq[Seq[Int]], row: Int, column: Int)

  def dripHeadRow(grid: Grid)(i: Int) = Grid(grid.digits.drop(i), grid.row - i, grid.column)

  def contains(original: Grid, objective: Grid): Boolean = {

    if(original.column >= objective.column && original.row >= objective.row) {
      val result = objective.digits zip original.digits map { case (objectiveRow, originalRow) =>
        originalRow.indexOfSlice(objectiveRow)
      }
      val head = result.head
      lazy val dropHeadColumn =
        contains(Grid(original.digits.map(_.drop(head + 1)), original.row, original.column - head - 1), objective)

      head >= 0 && (result.forall(_ == head) || dropHeadColumn)
    } else {
      false
    }
  }

  def solution(original: Grid, objective: Grid) = {
    ((0 to (original.row - objective.row)).toStream
      map dripHeadRow(original)
      exists (original => contains(original, objective))
      )
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val k :: Nil = readListInt()
    val rt = 1 to k map { _ =>
      val original_row :: original_column :: Nil = readListInt()
      val original_didit = (1 to original_row map (_ => io.StdIn.readLine().map(_.toInt).toVector)).toVector

      val objective_row :: objective_column :: Nil = readListInt()
      val objective_didit = (1 to objective_row map (_ => io.StdIn.readLine().map(_.toInt).toVector)).toVector

      solution(
        Grid(original_didit, original_row, original_column),
        Grid(objective_didit.toStream, objective_row, objective_column)
      )
    }
    val out = rt map {
      case true  => "YES"
      case false => "NO"
    }
    println(out.mkString("\n"))
  }
}
