package functionalProgramming.functionalStructures

import java.io.{BufferedReader, InputStreamReader}

import utils.SetInt

/**
  * Created by yujieshui on 2016/8/7.
  */
object MirkoAtTheConstructionSite {

  final case class Line(pitch: Int, slope: Int, index: Int)

  final case class Point(x: Double, y: Double)

  def intersectionPoint(a: Line, b: Line): Point = {
    val x = (a.pitch.toDouble - b.pitch) / (b.slope.toDouble - a.slope)
    Point(x, a.slope * x + a.pitch)
  }

  def buildDay(day: Int)(cs: Line) = cs.pitch + cs.slope * day

  def startLine(lines: Seq[Line]): Line = lines.maxBy(line => (buildDay(0)(line), line.index))

  def cutout(lines: Seq[Line])(min: Line) = lines.filter(_.slope > min.slope)

  def nextLine(lines: Seq[Line])(startLine: Line, startPoint: Point): (Point, Line) = {
    val (point, line) = lines.filter(_.slope > startLine.slope)
      .map { line => intersectionPoint(startLine, line) -> line }
      .minBy { case (point, line) => point.x }

    point -> line
  }

  final case class LineRange(line: Line, start: Point, end: Point)

  def impl(seq: Seq[Line], startLine: Line, startPoint: Point): Seq[LineRange] = {
    val maybeLines = cutout(seq)(startLine)
    if (maybeLines.isEmpty)
      Seq(LineRange(startLine, startPoint, Point(Double.MaxValue, Double.MaxValue)))
    else {
      val (nextPoint, nextLine) = this.nextLine(maybeLines)(startLine, startPoint)
      LineRange(startLine, startPoint, nextPoint) +: impl(maybeLines, nextLine, nextPoint)
    }
  }

  def solution2(seq: Seq[Line], query: Seq[Int]) = {
    val lineRange = impl(seq, startLine(seq), Point(0, 0))
    query.map(i => {
      lineRange.filter(e => e.start.x <= i && i <= e.end.x).maxBy(_.line.index).line.index
    })
  }

  final def solution(seq: Seq[Line], query: Seq[Int]): Seq[Int] = {
    query.map(day => {
      seq.maxBy(line => buildDay(day)(line) -> line.index).index
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
      builds.indices zip builds zip ondDayBuild map { case ((i, f), b) => Line(f, b, i + 1) }

    val out = solution2(seq, query)

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
