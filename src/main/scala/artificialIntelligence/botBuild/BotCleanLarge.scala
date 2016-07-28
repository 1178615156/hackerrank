package artificialIntelligence.botBuild

import utils.SetInt

/**
  * Created by yuJieShui on 2016/7/28.
  */

import BotEntity._

object BotCleanLarge {

  val dirty = 'd'

  def asDirty(point: Point, grid: Grid[Char]): Boolean = {
    grid.cell(x = point.x, y = point.y).value == dirty
  }

  def nextMovePoint(robotPoint: Point, grid: Grid[Char]): Point = {
    grid.find(dirty).map(e => e.point - robotPoint).sortBy(_.distance).head + robotPoint
  }

  def nextMove(robotPoint: Point, grid: Grid[Char]): Move.Value = {
    if (asDirty(robotPoint, grid)) {
      Move.CLEAN
    } else {
      val nextPoint = nextMovePoint(robotPoint, grid)
      moveStep(from = robotPoint, to = nextPoint).head
    }
  }


  def main(args: Array[String]) = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val robot_y :: robot_x :: Nil = readListInt()
    val grid_height :: grid_width :: Nil = readListInt()
    val in_data = 0 until grid_height map (height => {
      val line = io.StdIn.readLine()
      val cells = line.indices zip line map { case (weight, value: Char) => Cell(x = weight, y = height, value = value) }
      cells
    })
    val robotPoint = Point(x = robot_x, y = robot_y)
    val grid = Grid(in_data: _*)
    val out = nextMove(robotPoint, grid)
    println(
      out.toString
    )
  }

  //  SetInt(
  //    """0 0
  //      |5 5
  //      |b---d
  //      |-d--d
  //      |--dd-
  //      |--d--
  //      |----d
  //    """.stripMargin)
  SetInt(
    """0 1
      |5 5
      |-b--d
      |-d--d
      |--dd-
      |--d--
      |----d
      |
    """.stripMargin)
  SetInt(
    """0 0
      |5 5
      |bd---
      |-d---
      |---d-
      |---d-
      |--d-d
      |
    """.stripMargin)
}
