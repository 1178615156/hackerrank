package artificialIntelligence.botBuild

/**
  * Created by yuJieShui on 2016/1/1.
  */

object BotEntity {

  case class Point(@deprecatedName('high) x: Int,
                   @deprecatedName('weight) y: Int) {
    def high = y

    def weight = x

    def xx = x

    def yy = y

    def distance = this.x * this.x + this.y * this.y

    def skip = y + x

    def *(paint: Point) = Point(this.x * paint.x, this.y * paint.y)

    def -(paint: Point) = Point(this.x - paint.x, this.y - paint.y)

    def +(paint: Point) = Point(this.x + paint.x, this.y + paint.y)

    lazy val length = math.sqrt(x * x + y * y)

  }


  case class Cell[Value](@deprecatedName('high) x: Int,
                         @deprecatedName('weight) y: Int,
                         value: Value) {
    def high = y

    def weight = x

    val point = Point(x = x, y = y)
    lazy val distance = this.x * this.x + this.y * this.y
    lazy val skip     = y + x
  }

  case class Grid[Value](lines: Seq[Cell[Value]]*) {
    def cell(x: Int, y: Int): Cell[Value] = lines(y)(x)

    def line(y: Int) = lines(y)

    def row(x: Int) = lines.map(_ (x))

    def updateCell(new_cell: Cell[Value], x: Int, y: Int) = {
      val (line_head, line_tail) = lines.splitAt(y)
      val (cell_head, cell_tail) = line_tail.head.splitAt(x)
      val new_line = (cell_head :+ new_cell) ++ cell_tail.tail
      val new_grid = (line_head :+ new_line) ++ line_tail.tail
      Grid(new_grid: _*)
    }

    def find(value: Value): Seq[Cell[Value]] = lines.flatMap(_.filter(_.value == value))
  }

  object Move extends scala.Enumeration {
    val LEFT  = Value("LEFT")
    val RIGHT = Value("RIGHT")
    val UP    = Value("UP")
    val DOWN  = Value("DOWN")


    val CLEAN = Value("CLEAN")
  }

  def moveStep(from: Point, to: Point, rt: List[Move.Value] = Nil): List[Move.Value] = {
    if (to == from) rt
    else if (from.xx > to.xx) moveStep(to, from.copy(x = from.xx - 1), rt :+ Move.LEFT)
    else if (from.xx < to.xx) moveStep(to, from.copy(x = from.xx + 1), rt :+ Move.RIGHT)
    else if (from.yy > to.yy) moveStep(to, from.copy(y = from.yy - 1), rt :+ Move.UP)
    else if (from.yy < to.yy) moveStep(to, from.copy(y = from.yy + 1), rt :+ Move.DOWN)
    else ???
  }
}
