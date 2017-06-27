package utils


/**
  * Created by yujieshui on 2017/5/26.
  */
object Utils {

  implicit class WithSearch[A](coll: Seq[A]) {

    import scala.annotation.tailrec
    import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
    import scala.math.Ordering

    final def search[B >: A](elem: B)(implicit ord: Ordering[B]): SearchResult = binarySearch(elem, 0, coll.length)(ord)

    @tailrec
    private def binarySearch[B >: A](elem: B, from: Int, to: Int)(implicit ord: Ordering[B]): SearchResult = {
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

}
