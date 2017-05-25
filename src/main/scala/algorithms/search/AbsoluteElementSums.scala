package algorithms.search


/**
  * Created by yujieshui on 2017/5/25.
  */
object AbsoluteElementSums {

  import scala.annotation.tailrec
  import scala.math.Ordering
  import scala.collection.Searching.{Found, InsertionPoint, SearchResult}


  implicit class WithSearch[A](coll: Seq[A]) {


    final def search[B >: A](elem: B)(implicit ord: Ordering[B]): SearchResult =
      binarySearch(elem, 0, coll.length)(ord)

    @tailrec
    private def binarySearch[B >: A](elem: B, from: Int, to: Int)
                                    (implicit ord: Ordering[B]): SearchResult = {
      if(to == from) InsertionPoint(from) else {
        val idx = from + (to - from - 1) / 2
        math.signum(ord.compare(elem, coll(idx))) match {
          case -1 => binarySearch(elem, from, idx)(ord)
          case 1  => binarySearch(elem, idx + 1, to)(ord)
          case _  => Found(idx)
        }
      }
    }
  }

  def cumulate(seq: Seq[Long]) = {
    val array = new Array[Long](seq.size)
    var i = 0
    var sum = 0L
    seq.foreach { e =>
      sum += e
      array(i) = sum
      i += 1
    }
    array
  }

  def solution(elements: Seq[Long], queries: Seq[Long]): Seq[Long] = {
    val queryCumn = cumulate(queries)
    val less = elements.filter(_ < 0).map(-_).sorted
    val big = elements.filter(_ >= 0).sorted
    val lessCsum = cumulate(less)
    val bigCsum = cumulate(big)

    val bigSum = big.sum
    val lessSum = less.sum
    val sum = bigSum + lessSum
    val lessSize = less.size
    val bigSize = big.size
    queryCumn.map {
      case q if q >= 0 =>
        less.search(q) match {
          case Found(i)          =>
            val n = i + 1
            lessSum - 2 * lessCsum(n - 1) - lessSize * q + 2 * n * q + bigSum + bigSize * q
          case InsertionPoint(0) =>
            sum + lessSize * -q + bigSize * q
          case InsertionPoint(i) =>
            val n = i
            lessSum - 2 * lessCsum(n - 1) - lessSize * q + 2 * n * q + bigSum + bigSize * q
        }
      case q if q < 0  =>
        big.search(-q) match {
          case Found(i)          =>
            val n = i + 1
            bigSum - 2 * bigCsum(n - 1) + bigSize * q + 2 * n * -q + lessSum + lessSize * -q
          case InsertionPoint(0) =>
            sum + lessSize * -q + bigSize * q
          case InsertionPoint(i) =>
            val n = i
            bigSum - 2 * bigCsum(n - 1) + bigSize * q + 2 * n * -q + lessSum + lessSize * -q
        }
    }

  }

  object ReadIo {

    import java.io.BufferedReader
    import java.io.InputStreamReader

    val br = new BufferedReader(new InputStreamReader(System.in),1 << 16)
  }

  def readListInt() = {
    ReadIo.br.readLine().split(" ").map(_.toLong)
  }

  def main(args: Array[String]): Unit = {
    readListInt()
    val elements = readListInt()
    readListInt()
    val queries = readListInt()

    ReadIo.br.close()

    val result = solution(elements, queries)
    println(result.mkString("\n"))
  }
}
