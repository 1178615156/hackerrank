package artificialIntelligence.botBuild

import utils.SetInt

/**
  * Created by yuJieShui on 2016/1/1.
  */
object SavePrincess2 {


  SetInt(
    """
      |5
      |2 3
      |-----
      |-----
      |p--m-
      |-----
      |-----
    """.stripMargin)

  case class Point(yy: Int, xx: Int)

  def main(args: Array[String]) = {
    val sc = new java.util.Scanner(System.in);
    val m = sc.nextInt();
    val toX = sc.nextInt()
    val toY = sc.nextInt()
    sc.nextLine()
    val grid = new Array[String](m)
    for (i <- 0 until m) {
      grid.update(i, sc.nextLine())
    }
    nextMove(m, toX, toY, grid)
  }

  object Move extends scala.Enumeration {
    val LEFT  = Value("LEFT")
    val RIGHT = Value("RIGHT")
    val UP    = Value("UP")
    val DOWN  = Value("DOWN")
  }

  /* Refer to Output format section for more details */
  def nextMove(player: Int, r: Int, c: Int, grid: Array[String]): Unit = {
    def find[T](value: T, data: Seq[Seq[T]]): IndexedSeq[Point] = {
      val index = data.indices zip data map {
        case (index, data) ⇒
          Option(data.indexOf(value)).flatMap(e ⇒ if (e > -1) Some(e) else None).map(e ⇒ Point(index, e))
      } filter (_.nonEmpty) map (_.get)
      index
    }
    val princessPostition = find('p', grid.toSeq.map(e ⇒ e: Seq[Char]))
    val myPostition = find('m', grid.toSeq.map(e ⇒ e: Seq[Char]))

    def run(p: Point, my: Point, rt: List[Move.Value]): List[Move.Value] = {
      if (p == my) rt
      else if (my.xx > p.xx) run(p, my.copy(xx = my.xx - 1), rt :+ Move.LEFT)
      else if (my.xx < p.xx) run(p, my.copy(xx = my.xx + 1), rt :+ Move.RIGHT)
      else if (my.yy > p.yy) run(p, my.copy(yy = my.yy - 1), rt :+ Move.UP)
      else if (my.yy < p.yy) run(p, my.copy(yy = my.yy + 1), rt :+ Move.DOWN)
      else ???
    }
    val result = run(princessPostition.head, myPostition.head, Nil)
    println(result.head)
    //    result.foreach(e ⇒ println(e.toString))
  }
}
