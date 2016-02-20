package artificialIntelligence.botBuild

/**
  * Created by yuJieShui on 2016/1/1.
  */

object BotUtil {

  case class Point(yy: Int, xx: Int) {
    private val pos      = this
    lazy    val distance = pos.xx * pos.xx + pos.yy * pos.yy
    lazy    val skip     = yy + xx
  }

  def find[T](value: T, data: Seq[Seq[T]]): IndexedSeq[Point] = {
    val index = data.indices zip data map {
      case (index, data: Seq[T]) ⇒
        data.foldLeft((0, List[Point]())) { (l, r: T) ⇒
          if (r == value)
            (l._1 + 1, l._2 :+ Point(index, l._1))
          else
            (l._1 + 1, l._2)
        }._2
    }
    index.flatten
  }

  object Move extends scala.Enumeration {
    val LEFT  = Value("LEFT")
    val RIGHT = Value("RIGHT")
    val UP    = Value("UP")
    val DOWN  = Value("DOWN")
  }

  def moveStep(p: Point, my: Point, rt: List[Move.Value]): List[Move.Value] = {
    if (p == my) rt
    else if (my.xx > p.xx) moveStep(p, my.copy(xx = my.xx - 1), rt :+ Move.LEFT)
    else if (my.xx < p.xx) moveStep(p, my.copy(xx = my.xx + 1), rt :+ Move.RIGHT)
    else if (my.yy > p.yy) moveStep(p, my.copy(yy = my.yy - 1), rt :+ Move.UP)
    else if (my.yy < p.yy) moveStep(p, my.copy(yy = my.yy + 1), rt :+ Move.DOWN)
    else ???
  }

  type Distance = Int

  def minSkip(goal: Point, data: Seq[Point]): (Seq[Point], Distance) = {
    val dataDistance: Seq[Distance] = data.map(e ⇒ math.abs(
      math.abs(e.xx - goal.xx) + math.abs(e.yy - goal.yy)
    ))

    val dataDistanceTail: Seq[Seq[Point]] = data.map(e ⇒ data.filterNot(_ == e))

    val subDataMinSkip: Seq[(Seq[Point], Distance)] = (data, dataDistance, dataDistanceTail).zipped map {
      case (e, distance, tail) ⇒

        val (tailSort, tailDistance) =
          if (tail.isEmpty)
            List() → 0
          else {
            minSkip(e, tail)
          }
        (e +: tailSort, tailDistance + distance)
    }
    subDataMinSkip.minBy(_._2)

  }

}
