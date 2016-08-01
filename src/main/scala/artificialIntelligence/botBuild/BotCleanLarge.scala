package artificialIntelligence.botBuild

import utils.SetInt

/**
  * Created by yuJieShui on 2016/7/28.
  */


object BotCleanLarge {


  import BotEntity._

  val dirty = 'd'

  def asDirty(point: Point, grid: Grid[Char]): Boolean =
    grid.cell(x = point.x, y = point.y).value == dirty

  def skip(start: Point, seq: Seq[Point]): Int =
    if (seq.isEmpty) 0 else skip(start, seq.head) + skip(seq.head, seq.tail)

  def skip(start: Point, end: Point): Int =
    math.abs(end.x - start.x) + math.abs(end.y - start.y)

  case class PointSkip(point: Point, skip: Int)

  case class Work(beforeList: Seq[Point], point: Point, dirtys: Seq[Point],skip:Int=0)

  def search(workList: Seq[Work]): Seq[Point] = {
    if (workList.exists(_.dirtys.isEmpty))
      workList.map(w=>( w.point +: w.beforeList).reverse ).minBy(seq=> skip(seq.head,seq.tail))
    else{
      val clean_work_list = workList//.sortBy(work => skip(work.point,work.dirtys.head))
      val new_work: Seq[Work] =clean_work_list.flatMap {
        case Work(beforeList, point, dirtys,skip) =>

          dirtys.map(dirty => Work(point +: beforeList , dirty, dirtys.filter(_ != dirty),skip + this.skip(point,dirty)))
      }.sortBy(_.skip).take(7)
      search(new_work)
    }
  }

  def greedy(point: Point, dirtys: Seq[Point]): Seq[Point] =
    if (dirtys.isEmpty) Seq()
    else {
      val head: Point = dirtys.minBy(dirty => (skip(point, dirty), dirty.x, dirty.y))
      head +: greedy(head, dirtys.filter(_ != head))
    }

  def bfs(point: Point, dirtys: Seq[Point], nowSkip: Int = 0, minSkip: Int = Int.MaxValue): Option[Seq[Point]] = {
    val need_compute_dirty = dirtys.filter(dirty => nowSkip + skip(point, dirty) <= minSkip)
    if (dirtys.isEmpty)
      Some(Seq())
    else if (need_compute_dirty.isEmpty)
      None
    else if (dirtys.size <= 100) {
      val result = greedy(point, dirtys)
      if (skip(point, result) + nowSkip <= minSkip) Some(result) else None
    }
    else {
      val final_dirty =
        need_compute_dirty.groupBy(dirty => skip(point, dirty)).toSeq.sortBy(_._1).take(1).flatMap(_._2).take(5)

      val result = final_dirty.map(dirty => bfs(
        point = dirty,
        dirtys = dirtys.filter(_ != dirty),
        nowSkip = nowSkip + skip(point, dirty),
        minSkip).map(dirty +: _)).filter(_.nonEmpty).map(_.get)
      if (result.isEmpty) None else Option(result.minBy(seq => skip(point, seq)))
    }
  }

  def nextMovePoint(robotPoint: Point, grid: Grid[Char]): Point = {

    val dirtys = grid.find(dirty).map(_.point)
    //    val result = bfs(robotPoint, dirtys, nowSkip = 0, minSkip = skip(robotPoint, greedy(robotPoint, dirtys)))
    //    result.get.head
//    greedy(robotPoint, dirtys).head

    search(Seq(Work(Seq(),robotPoint,dirtys,0))).tail.head
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

}


//  SetInt(
//    """0 1
//      |5 5
//      |-b--d
//      |-d--d
//      |--dd-
//      |--d--
//      |----d
//      |
//    """.stripMargin)
//SetInt(
//"""0 0
//  |5 5
//  |bd---
//  |-d---
//  |---d-
//  |---d-
//  |--d-d
//  |
//""".stripMargin)