package algorithms.search

import scala.collection.{immutable, mutable}

/**
  * Created by yujieshui on 2017/6/27.
  */
object SimilarPair {

  case class Node(parent: Int, value: Int)

  def root(n: Int, seq: Seq[Node]): Seq[Int] =
    (1 to n) diff (seq.map(_.value))

  def leaf(n: Int, seq: Seq[Node]): Seq[Int] =
    (1 to n) diff (seq.map(_.parent))

  def toParentMap(seq: Seq[Node]): Map[Int, Int] =
    seq.map(e => e.value -> e.parent).toMap

  def toClientMap(seq: Seq[Node])=
    seq.groupBy(_.parent).mapValues(_.map(_.value))
  def parentLink2Map(work: Int,
                     parentMap: Map[Int, Int],
                     clientMap: Map[Int, Seq[Int]],
                     result: Map[Int, Seq[Int]]): Map[Int, Seq[Int]] = {

    val new_result =
      if(parentMap.get(work).isEmpty)
        result
      else
        result + (work -> (work +: result(parentMap(work))))
    if(clientMap.get(work).isEmpty) new_result
    else
      clientMap(work).foldLeft(new_result) {
        case (acc, element) => parentLink2Map(element, parentMap, clientMap, acc)
      }

  }

  import scala.collection.Searching.{Found, InsertionPoint, SearchResult}


  def solution(n: Int, k: Int, seq: Seq[Node]): Seq[Int] = {
    val parentMap = toParentMap(seq)
    val parentLinkMap =
      parentLink2Map(root(n, seq).head, parentMap, toClientMap(seq), Map(root(n, seq).map(e => e -> Seq(e)): _*))
      .mapValues(_.tail.sorted)
    1 to n map { e =>
      parentLinkMap(e).search(e - k) -> parentLinkMap(e).search(e + k) match {
        case (Found(start), Found(end))                   =>
          end - start + 1
        case (Found(start), InsertionPoint(end))          =>
          end - start
        case (InsertionPoint(start), Found(end))          =>
          end - start + 1
        case (InsertionPoint(start), InsertionPoint(end)) =>
          end - start
      }
    }
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: k :: Nil = readListInt()
    val nodes = 1 until  n map (e => readListInt() match {
      case a :: b :: Nil => Node(a, b)
    })
    val result = solution(n, k, nodes).sum
    println(result)
  }

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
