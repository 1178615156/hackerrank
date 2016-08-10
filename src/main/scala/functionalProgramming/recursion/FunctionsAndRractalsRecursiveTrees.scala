package functionalProgramming.recursion

/**
  * Created by yujieshui on 2016/8/10.
  */
object FunctionsAndRractalsRecursiveTrees {

  case class Cell[Value](@deprecatedName('high) x: Int,
                         @deprecatedName('weight) y: Int,
                         value: Value)

  case class Grid[Value](lines: Seq[Cell[Value]]*) {
    def cell(x: Int, y: Int): Cell[Value] = lines(y)(x)

    def line(y: Int) = lines(y)

    def row(x: Int) = lines.map(_ (x))

    def updateCell(new_cell: Cell[Value]) = {
      val x = new_cell.x
      val y = new_cell.y
      val (line_head, line_tail) = lines.splitAt(y)
      val (cell_head, cell_tail) = line_tail.head.splitAt(x)
      val new_line = (cell_head :+ new_cell) ++ cell_tail.tail
      val new_grid = (line_head :+ new_line) ++ line_tail.tail
      Grid(new_grid: _*)
    }

    def find(value: Value): Seq[Cell[Value]] = lines.flatMap(_.filter(_.value == value))

    override def toString: String = {
      lines.map(_.map(_.value).mkString("")).mkString("\n")
    }
  }

  case class Point(start_row: Int, start_column: Int, size: Int)

  def init = Grid(1 to 63 map { row => 1 to 100 map { column => Cell(column, row, '_') } }: _*)

  def up(point: Point): Grid[Char] = ???

  def left(point: Point): Grid[Char] = ???

  def right(point: Point): Grid[Char] = ???

  def tree(point: Point)(grid: Grid[Char]): Grid[Char] = ???

  def endPoint(grid: Grid[Char]): Seq[Point] = ???

  def fistPoint: Point = Point(0, 50, -1)


  def solution(grid: Grid[Char], point: Point) = {

  }

  def main(args: Array[String]): Unit = {
    println(init)
  }
}
