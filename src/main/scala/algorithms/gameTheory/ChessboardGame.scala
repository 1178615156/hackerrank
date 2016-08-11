package algorithms.gameTheory

/**
  * Created by yujieshui on 2016/8/11.
  */
object ChessboardGame {

  object Play extends Enumeration {
    val First  = Value("First")
    val Second = Value("Second")
    type Play = Value

    def switchPlay(play: Play) = play match {
      case First  => Second
      case Second => First
    }
  }

  import Play._

  case class Point(x: Int, y: Int)

  val initCacheMap: Map[Point, Play] = {
    val points = for {
      x <- 1 to 15
      y <- 1 to 15
    } yield Point(x, y)

    points.sortBy(e => e.x + e.y).foldLeft(Map[Point, Play]()) {
      case (acc, point) =>
        acc + (point -> s2(point, First, acc))
    }
  }

  def nextPoint(point: Point): Seq[Point] = {
    import point._
    Seq(
      Point(x - 2, y - 1),
      Point(x - 1, y - 2),
      Point(x + 1, y - 2),
      Point(x - 2, y + 1)
    ).filter(e => e.x > 0 && e.y > 0 && e.x <= 15 && e.y <= 15)
  }

  def s2(point: Point, play: Play, cache: Map[Point, Play]): Play = {
    if (cache.keys.exists(_ == point))
      if (play == First) cache(point) else switchPlay(cache(point))
    else {
      val nextPoints = nextPoint(point)
      if (nextPoints.isEmpty)
        switchPlay(play)
      else {
        val all = nextPoints.map(point => s2(point, switchPlay(play), cache))
        if (all.contains(play)) play else switchPlay(play)
      }
    }
  }

  def solution(point: Point, play: Play): Play = {
    initCacheMap(point)
  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val out =
      1 to n map (_ => readListInt()) map (e => Point(e(0), e(1))) map (solution(_, Play.First))
    println(out mkString "\n")
  }
}
