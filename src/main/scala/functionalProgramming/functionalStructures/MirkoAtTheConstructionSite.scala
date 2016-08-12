package functionalProgramming.functionalStructures

import java.io.{BufferedReader, InputStreamReader}

import utils.SetInt

/**
  * Created by yujieshui on 2016/8/7.
  */
object MirkoAtTheConstructionSite {

  case class Line(pitch: Int, slope: Int, index: Int)

  case class Point(x: Double, y: Double)

  def intersectionPoint(a: Line, b: Line): Point = {
    val x = (b.slope - a.slope) / (a.pitch - b.pitch)
    Point(x, a.slope * x + a.pitch)
  }

  def max(lines: Seq[Line]): Line = lines.maxBy(e => (e.pitch, e.index))

  def find_max_line(lines: Seq[Line]) = {
    val (max_line) = max(lines)

    val (x, next_lines) = lines.filter(_.slope > max_line.slope)
      .groupBy(e => math.floor(intersectionPoint(e, max_line).x).toInt)
      .minBy(_._1)

  }

  def solution2(lines: Seq[Line], query: Seq[Int]) = {

  }

  def buildDay(day: Int)(cs: Line) =
    cs.copy(pitch = cs.pitch + cs.slope * day)

  def solution(seq: Seq[Line], query: Seq[Int]): Seq[Int] = {
    query.map(day => {
      val x = seq.indices zip seq.map(buildDay(day))
      val (index, _) = x.maxBy(e => (e._2.pitch, e._1))
      index + 1
    })
  }

  def main(args: Array[String]): Unit = {
    val bi = new BufferedReader(new InputStreamReader(System.in))
    def readListInt() = bi.readLine().split(" ").toList.map(_.toInt)
    val n :: q :: Nil = readListInt()
    val builds = readListInt()
    val ondDayBuild = readListInt()
    val query = 1 to q map (_ => bi.readLine().toInt)

    val seq =
      builds.indices zip builds zip ondDayBuild map { case ((i, f), b) => Line(f, b, i) }

    val out = solution(seq, query)

    println(
      out.mkString("\n")
    )
  }

  SetInt(
    """3 6
      |7 5 1
      |1 2 3
      |0
      |1
      |2
      |3
      |4
      |5
      |
    """.stripMargin)
}
