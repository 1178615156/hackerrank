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

  implicit def OrderingPoint: Ordering[Point] = Ordering.by((e: Point) => e.row -> e.column)

  def empty = Seq[Point]()


  def cover(left: Point, right: Point) = {
    left.row == right.row ||
      left.column == right.column ||
      math.abs(right.row - left.row) == math.abs(right.column - left.column)
  }

  def someLine(a: Point, b: Point, c: Point): Boolean = {
    (b - a).slope == (c - a).slope
  }

  def someLine(a: Point, seq: Seq[Point]): Boolean = {
    if(seq.isEmpty) false
    else {
      val b = seq.head
      seq.tail.exists(c => someLine(a, b, c)) || someLine(a, seq.tail)
    }
    //    seq.exists(b => seq.filter(_ != b).exists(c => someLine(a, b, c)))
  }


  def solution(workList: Seq[Point], i: Int, n: Int): Option[Seq[Point]] = {
    if(i > n) Some(workList)
    else {
      val currentLayer =
        (1 to n).toStream
          .map(j => Point(i, j))
          .filterNot(p => workList.exists(p2 => cover(p, p2)) || someLine(p, workList))
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
    val n = 21
    val result = solution(empty, 1, n).get
    println(s"println($n)")
    println(
      s"""println("${result.map(_.column).reverse.mkString(" ")}")"""
    )
  }
}
