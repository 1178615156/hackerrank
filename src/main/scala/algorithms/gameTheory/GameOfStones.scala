package algorithms.gameTheory

import utils.SetInt

/**
  * Created by yujieshui on 2016/8/10.
  */
object GameOfStones {

  object Play extends Enumeration {
    val First  = Value("First")
    val Second = Value("Second")
  }

  def nextPlay(play: Play.Value) = play match {
    case Play.First  => Play.Second
    case Play.Second => Play.First
  }

  def upPlay(play: Play.Value) = nextPlay(play)

  type Stones = Int

  def remove(i: Int)(n: Stones): Stones = n - i

  val remove2 = remove(2) _
  val remove3 = remove(3) _
  val remove5 = remove(5) _

  def isEnd(n: Int) = n <= 5

  val cacheMap = (1 to 100).foldLeft(Map(
    1 -> Play.Second,
    2 -> Play.First,
    3 -> Play.First,
    4 -> Play.First,
    5 -> Play.First
  )) { case (acc, n) => acc + (n -> s2(acc, n, Play.First)) }

  def s2(map: Map[Int, Play.Value], n: Stones, play: Play.Value): Play.Value = {
    if (map.keys.exists(_ == n))
      if (play == Play.First) map(n) else nextPlay(map(n))
    else if (n <= 1) upPlay(play)
    else {
      val all = Seq(
        s2(map, remove2(n), nextPlay(play)),
        s2(map, remove3(n), nextPlay(play)),
        s2(map, remove5(n), nextPlay(play))
      )
      if (all.contains(play)) play else upPlay(play)
    }
  }

  def solution(n: Stones, play: Play.Value): Play.Value = {
    cacheMap(n)
  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val out =
      1 to n map (_ => io.StdIn.readLine().toInt) map (n => solution(n, Play.First))

    println(out map (_.toString) mkString "\n")
  }


}
