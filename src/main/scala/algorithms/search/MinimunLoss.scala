package algorithms.search

/**
  * Created by yujieshui on 2017/6/21.
  */
object MinimunLoss {

  case class House(year: Int, price: Long)

  def impl(seq: Seq[House], rt: Seq[Option[Long]] = Nil): Long = {
    if(seq.isEmpty) rt.collect { case Some(x) => x }.min
    else {
      val head = seq.head
      val minLoss = seq.find { case House(year, price) => year > head.year && price < head.price}
      impl(seq.tail, minLoss.map(head.price - _.price) +: rt)
    }
  }

  def solution(seq: Seq[Long]) = {
    impl(seq.indices zip seq map { case (i, p) => House(i, p) } sortBy(e=> -e.price))
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toLong)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val ps = readListInt()
    val result = solution(ps)
    println(result)
  }
}
