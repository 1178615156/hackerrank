package artificialIntelligence.botBuild

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

import utils.SetInt


object BotClean {

  import BotUtil._

  def main(args: Array[String]) = {
    val sc = new java.util.Scanner(System.in)
    val y = sc.nextInt()
    val x = sc.nextInt()
    sc.nextLine()
    val board = new Array[String](5)
    for (i <- 0 until 5) {
      board.update(i, sc.nextLine())
    }
    nextMove(Point(xx = x, yy = y), board)
  }

  def nextMove(pos: Point, board: Array[String]) = {
    val dirtyPostition = find('d', board.toSeq.map(e => e: Seq[Char]))

    val out: (Seq[Point], Distance) = minSkip(pos, dirtyPostition)
    println(moveStep(out._1.head, pos, Nil).headOption.map(_.toString).getOrElse("CLEAN"))
  }

  SetInt(
    """
      |2 3
      |---dd
      |-----
      |---b-
      |d----
      |d---d
    """.stripMargin)

}





























