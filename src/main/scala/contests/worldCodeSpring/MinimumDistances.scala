package contests.worldCodeSpring


/**
  * Created by yuJieShui on 2016/6/26.
  */
object MinimumDistances {


  def solution(seq: Seq[Int],
               i: Int,
               startIndex: Map[Int, Int],
               historyDistances: Map[Int, Int]): Int = seq match {
    case Nil          =>
      scala.util.Try(historyDistances.minBy(_._2)._2).getOrElse(-1)
    case head :: tail =>
      startIndex.get(head) match {
        case Some(start) =>
          val now_distance = i - start
          val distance =
            historyDistances.get(head)
              .map(before => math.min(before, now_distance)).getOrElse(now_distance)
          solution(tail, i + 1, startIndex + (head -> i), historyDistances + (head -> distance))
        case None        =>
          solution(tail,
            i + 1,
            startIndex + (head -> i),
            historyDistances)
      }

  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val data = readListInt()
    println(
      solution(
        data, 0, Map(), Map()
      )
    )
  }
}
