package artificialIntelligence.botBuild

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

import utils.SetInt


object BotClean {

  import BotUtil._

  def main(args: Array[String]) = {
    val sc = new java.util.Scanner(System.in);
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

object BotCleanLarge {
  SetInt(
    """
      |0 0
      |5 6
      |b---d-
      |-d--d-
      |--dd--
      |--d---
      |----d-
    """.stripMargin)

  import BotUtil._

  def main(args: Array[String]) = {
    val sc = new java.util.Scanner(System.in);
    val y = sc.nextInt()
    val x = sc.nextInt()
    val h = sc.nextInt()
    val w = sc.nextInt()
    sc.nextLine()
    val board = 1 to h map (_ ⇒ (sc.nextLine()))
    nextMove(Point(xx = x, yy = y), board.toArray)
  }

  def nextMove(pos: Point, board: Array[String]) = {
    val dirtyPostition = find('d', board.toSeq.map(e => e: Seq[Char]))

    val out: (Seq[Point], Distance) = minSkip(pos, dirtyPostition)
    println(moveStep(out._1.head, pos, Nil).headOption.map(_.toString).getOrElse("CLEAN"))
  }
}

object BitCLeanPartiallyObservable {
  SetInt.apply(
    """
      |
      |4 3
      |ooooo
      |ooooo
      |ooooo
      |oo---
      |oo---
    """.stripMargin)

  import BotUtil._

  def main(args: Array[String]) = {
    val sc = new java.util.Scanner(System.in);
    val x = sc.nextInt()
    val y = sc.nextInt()
    val h = 5 //sc.nextInt()
    val w = 5 //sc.nextInt()
    sc.nextLine()
    val board = 1 to h map (_ ⇒ (sc.nextLine()))
    nextMove(Point(x, y), board.toArray)
  }

  def nextMove(pos: Point, board: Array[String]) = {
    val dirtyPostition = find('d', board.toSeq.map(e => e: Seq[Char]))
    if (dirtyPostition.isEmpty) {

      val x = (math.random * 4).toInt // if ((pos.xx + 1) % 5 == 0) 0 else pos.xx + 1
      val y = (math.random * 4).toInt // if ((pos.yy + 1) % 5 == 0) 0 else pos.yy + 1
      println(moveStep(Point(y, x), pos, Nil).headOption.map(_.toString).getOrElse("CLEAN"))

    } else {
      val out: (Seq[Point], Distance) = minSkip(pos, dirtyPostition)
      println(moveStep(out._1.head, pos, Nil).headOption.map(_.toString).getOrElse("CLEAN"))

    }


  }


}

























