package algorithms.search

/**
  * Created by yujieshui on 2017/5/22.
  */
object GridlandMetro {

  case class Train(r: Int, c: Int, l: Int) {
    def start = c

    def end = l

    def size = end.toLong - start + 1
  }

  def agg(seq: List[Train]): Seq[Train] = {
    seq match {
      case Nil             => seq
      case a :: Nil        => seq
      case a :: b :: other =>
        if(b.start <= a.end)
          agg(Train(a.r, a.start, math.max(a.end, b.end)) :: other)
        else
          a +: agg(b :: other)
    }
  }

  def solution(r: Int, c: Int, trains: Seq[Train]): Long = {
    val trainSize: Long = trains.groupBy(_.r).values.map { trains =>
      this
        .agg(trains.sortBy(_.start).toList)
        .map(_.size).sum
    }.sum
    r.toLong * c.toLong - trainSize

  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val r :: c :: k :: Nil = readListInt()
    val trains = 1 to k map (_ => readListInt()) map { case r :: c :: l :: Nil => Train(r, c, l) }
    val result = solution(r, c, trains)
    println(result)
  }
}
