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

  case class Tree[T](value: T, child: Seq[Tree[T]])

  def paintGridAround(paint: Paint[Char], grid: Grid[Char]) = {
    val paint_value = grid(paint.row)(paint.column)

    val up = grid(paint.row - 1)(paint.column)
    grid(paint.row - 1)(paint.column) = USING

    val left = grid(paint.row)(paint.column - 1)
    grid(paint.row)(paint.column - 1)= USING

    val right = grid(paint.row )(paint.column + 1)
    grid(paint.row - 1)(paint.column + 1)

    val down = grid(paint.row + 1)(paint.column)
    grid(paint.row + 1)(paint.column)= USING

    down :: right :: left :: up :: Nil
  }

  def mkTree(begin: Paint[Char], end: Paint[Char], grid: Grid[Char]) = {
    val begin= ???
  }

  def main(args: Array[String]): Unit = {
    val pacMan_row :: pacMain_column :: Nil = readListInt()
    val food_row :: food_column :: Nil = readListInt()
    val grid_row :: grid_column :: Nil = readListInt()

    val grid = readGrid(grid_row)
    println(grid map (_.mkString("")))
  }


  SetInt(
    """3 9
      |5 1
      |7 20
      |%%%%%%%%%%%%%%%%%%%%
      |%--------------%---%
      |%-%%-%%-%%-%%-%%-%-%
      |%--------P-------%-%
      |%%%%%%%%%%%%%%%%%%-%
      |%.-----------------%
      |%%%%%%%%%%%%%%%%%%%%
    """.stripMargin)
}
