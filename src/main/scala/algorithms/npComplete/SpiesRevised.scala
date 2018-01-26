package algorithms.npComplete

import scala.collection.immutable.TreeSet

/**
  * Created by yujieshui on 2017/7/12.
  */
object SpiesRevised {

  case class Point(row: Int, column: Int) {

    val slope = row.toDouble / column

    def -(other: Point) = Point(row - other.row, column - other.column)

  }

  implicit val OrderingPoint: Ordering[Point] = Ordering.by((e: Point) => e.row -> e.column)

  def empty =
  //    new Work(Nil, Set())
    List[Point]()


  def cover(left: Point, right: Point) = {
    left.row == right.row ||
      left.column == right.column ||
      math.abs(right.row - left.row) == math.abs(right.column - left.column)
  }

  def sameLine(a: Point, b: Point, c: Point): Boolean = {
    sameLine(a, List(b, c))
  }


  val sameLineCache = scala.collection.mutable.Map[(Point, List[Point]), Boolean]()

  def sameLine(a: Point, list: List[Point]): Boolean = {
    if(sameLineCache.contains(a -> list)) sameLineCache(a -> list)
    else {

      def exist_dup(list: List[Double]): Boolean = {
        list match {
          case Nil                   => false
          case a :: Nil              => false
          case a :: b :: _ if a == b => true
          case a :: tail             => exist_dup(tail)
        }
      }

      val slope = list.map(e => (e - a).slope).sorted
      val result = exist_dup(slope)
      sameLineCache += ((a -> list, result))
      result
    }
  }

  def solution(workList: List[Point], i: Int, n: Int): Option[Seq[Point]] = {
    if(i > n) Some(workList)
    else {
      val currentLayer =
        (1 to n).toStream
          .map(j => Point(i, j))
          .filterNot(p => workList.exists(p2 => cover(p, p2)))
          .filterNot(p => sameLine(p, workList))
      if(currentLayer.isEmpty)
        None
      else {
        currentLayer
          .map(p => solution(p +: workList, i + 1, n))
          .find(_.nonEmpty)
          .flatten
      }

    }
  }

  def main(args: Array[String]): Unit = {
    val n = 27
    println("1 3 6 2 7 10 23 26 18 15 24 19 9 25 21 8 12 5 27 13 20 4 17 14 22 11 16")
    println("1 3 6 2 14 19 15 10 20 17 4 9 7 18 21 12 16 8 5 11 13")

    val starTime = System.currentTimeMillis()
    val result = solution(empty, 1, n).get
    println(s"println($n)")
    println(
      s"""println("${result.map(_.column).reverse.mkString(" ")}")"""
    )
    val endTime = System.currentTimeMillis()
    println((endTime - starTime).toDouble / 1000)
  }
}
//  class Work(val list: List[Point], slops: Set[Double]) {
//
//    def +:(p: Point): Work = {
//      val new_list = p +: list
//      val new_slops = slops ++ list.map(e => (p - e).slope)
//      new Work(new_list, new_slops)
//    }
//
//    def exists(f: Point => Boolean): Boolean = list.exists(f)
//
//    def sameLine(point: Point): Boolean = {
//      val a = list.exists(cover(_, point))
//      //      val b = SpiesRevised.sameLine(point, list)
//      lazy val b = list.map(e => (point - e).slope).exists(slops.contains)
//      a || b
//    }
//
//  }
//
//  def solution(workList: Work, i: Int, n: Int): Option[Seq[Point]] = {
//    if(i > n) Some(workList.list)
//    else {
//      val currentLayer =
//        (1 to n).toStream
//          .map(j => Point(i, j))
//          .filterNot(p => workList.sameLine(p))
//      if(currentLayer.isEmpty)
//        None
//      else {
//        currentLayer
//          .map(p => solution(p +: workList, i + 1, n))
//          .find(_.nonEmpty)
//          .flatten
//      }
//
//    }
//  }