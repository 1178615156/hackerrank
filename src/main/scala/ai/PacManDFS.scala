package ai

import utils.{SetInt, Tool}

/**
  * Created by yuJieShui on 2016/3/17.
  */
object PacManDFS {

  import Tool._

  val FOOD    = '.'
  val PAC_MAN = 'P'
  val WALL    = '%'
  val PATH    = '-'
  val USING   = 'U'

  case class Tree[T](value: Option[T], child: Seq[Tree[T]]) {
    def isEmpty = value.isEmpty && child.isEmpty
  }

  object Tree {
    def empty[T] = new Tree[T](None, Nil)
  }


  def paintGridAround(paint: Paint[Char], grid: Grid[Char]) = {

    val up = Paint(paint.row - 1, paint.column, Some(grid(paint.row - 1)(paint.column)))
    grid(paint.row - 1)(paint.column) = USING

    val left = Paint(paint.row, paint.column - 1, Some(grid(paint.row)(paint.column - 1)))
    grid(paint.row)(paint.column - 1) = USING

    val right = Paint(paint.row, paint.column + 1, Some(grid(paint.row)(paint.column + 1)))
    grid(paint.row)(paint.column + 1) = USING

    val down = Paint(paint.row + 1, paint.column, Some(grid(paint.row + 1)(paint.column)))
    grid(paint.row + 1)(paint.column) = USING

    down :: right :: left :: up :: Nil
  }

  def mkTree(begin: Paint[Char], end: Paint[Char], grid: Grid[Char]): Tree[Paint[Char]] = {
    val begin_pint = Paint(row = begin.row, column = begin.column, value = Some(grid(begin.row)(begin.column)))

    val paintAround = paintGridAround(begin, grid) filter (_.value.exists(e => e != WALL && e != USING))
    val gridEnd = paintAround filter (e => e.row == end.row && e.column == end.column)
    if (gridEnd.nonEmpty)
      Tree(value = Some(begin_pint), child = paintAround map (e => Tree(Some(e), Nil)))
    else {
      Tree(Some(begin_pint), paintAround map (e => mkTree(e, end, grid)))
    }
  }

  type GridTree = Tree[Paint[Char]]

  def dfs(tree: Seq[GridTree], end: PaintChar, rt: Seq[PaintChar] = Nil): (Boolean, Seq[PaintChar]) = {
    if (tree.isEmpty)
      (false, rt.reverse)
    else if (tree.head.value.forall(e => e.row == end.row && e.column == end.column))
      (true, (tree.head.value.get +: rt).reverse)
    else {
      dfs(tree.head.child ++ tree.tail, end, tree.head.value.get +: rt)
    }
  }

  def minPath(tree: Seq[GridTree], end: PaintChar, rt: Seq[PaintChar] = Nil): Option[Seq[PaintChar]] = {
    if (tree.isEmpty)
      None
    else {
      val result = tree.head.value.get +: rt
      if (tree.head.value.forall(e => e.row == end.row && e.column == end.column))
        Some(result.reverse)
      else {
        val head_child = minPath(tree.head.child, end, result)
        if (head_child.nonEmpty)
          head_child
        else
          minPath(tree.tail, end, rt)
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val pacMan_row :: pacMain_column :: Nil = readListInt()
    val food_row :: food_column :: Nil = readListInt()
    val grid_row :: grid_column :: Nil = readListInt()

    val grid = readGrid(grid_row)
    val begin: Paint[Char] = Paint(pacMan_row, pacMain_column)
    val end: Paint[Char] = Paint(food_row, food_column)
    val tree = (mkTree(begin, end, grid))

    val (canFindEnd, dfsPath: Seq[PaintChar]) = dfs(Seq(tree), end)

    val minDfsPath = minPath(Seq(tree), end)
//    println(dfsPath.size.toString +: dfsPath.map(e => s"${e.row} ${e.column}") mkString "\n")
    println((minDfsPath.get.size - 1).toString +: minDfsPath.get.map(e => s"${e.row} ${e.column}") mkString "\n")
  }

SetInt(
  """11 9
    |2 15
    |13 20
    |%%%%%%%%%%%%%%%%%%%%
    |%----%--------%----%
    |%-%%-%-%%--%%-%.%%-%
    |%-%-----%--%-----%-%
    |%-%-%%-%%--%%-%%-%-%
    |%-----------%-%----%
    |%-%----%%%%%%-%--%-%
    |%-%----%----%-%--%-%
    |%-%----%-%%%%-%--%-%
    |%-%-----------%--%-%
    |%-%%-%-%%%%%%-%-%%-%
    |%----%---P----%----%
    |%%%%%%%%%%%%%%%%%%%%
  """.stripMargin
)
//  SetInt(
//    """3 9
//      |5 1
//      |7 20
//      |%%%%%%%%%%%%%%%%%%%%
//      |%--------------%---%
//      |%-%%-%%-%%-%%-%%-%-%
//      |%--------P-------%-%
//      |%%%%%%%%%%%%%%%%%%-%
//      |%.-----------------%
//      |%%%%%%%%%%%%%%%%%%%%
//    """.stripMargin)

}
