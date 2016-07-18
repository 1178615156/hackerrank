package functionalProgramming.introduction

import utils.SetInt

import scala.math.Pi

/**
  * Created by yujieshui on 2016/7/18.
  */
object ConcavePolygon {

  case class Paint(x: Double, y: Double) {
    def *(paint: Paint) = Paint(this.x * paint.x, this.y * paint.y)

    def -(paint: Paint) = Paint(this.x - paint.x, this.y - paint.y)

    def +(paint: Paint) = Paint(this.x + paint.x, this.y + paint.y)

    lazy val angle  = {
      if (x >= 0 && y == 0) 0
      else if (x == 0 && y > 0) Pi / 2
      else if (x < 0 && y == 0) Pi
      else if (x == 0 && y < 0) Pi + Pi / 2
      else (math.atan2(y,x) + (2* Pi)) % (2 * Pi)
//
//      else if (x > 0 && y > 0) math.atan2(x, y)
//      else if (x < 0 && y > 0) Pi / 2 + math.atan2(-x, y)
//      else if (x < 0 && y < 0) Pi + math.atan2(-x, -y)
//      else if (x > 0 && y < 0) Pi + Pi / 2 + math.atan2(x, -y)
//      else ???
    }
    lazy val length = math.sqrt(x * x + y * y)

    override def toString: String = {s"""x=$x,y=$y,angle=$angle,length=$length"""}
  }

  type Paints = Seq[Paint]

  case class Line(head: Paint, tail: Paint)

  case class Angle(left: Paint, mid: Paint, right: Paint) {
    val isLeft = {
      val a = left
      val b = right
      val p = mid
      (b.x - a.x) * (p.y - a.y) - (b.y - a.y) * (p.x - a.x) > 0
    }

    def isRight = !isLeft

    val angle = {
      val a = (left - mid)
      val b = (right - mid)
      math.max(a.angle, b.angle) - math.min(a.angle, b.angle)
    }

    override def toString: String = s"""angle=$angle"""
  }

  def centerPaintOf(paints: Paints) = {
    val r = paints reduce (_ + _)
    Paint(r.x / paints.size, r.y / paints.size)
  }

  def paintsToAngle(paints: Paints) = {
    def impl(paints: Paints, rt: Seq[Angle]): Seq[Angle] = {
      if (paints.size < 3)
        rt
      else {
        val Seq(left, mid, right) = paints.take(3)
        impl(paints.tail, Angle(left, mid, right) +: rt)
      }
    }
    val new_paints = paints ++ paints.take(2)
    impl(new_paints, Nil)
  }

  def solution(paints: Paints): Boolean = {
    val centerPaint = centerPaintOf(paints)
    val new_paints = (paints map (_ - centerPaint) sortWith ((l, r) => {
      if (l.angle < r.angle) true
      else if (l.angle == r.angle && l.length < r.length) true
      else false
    }))
    val angles = paintsToAngle(new_paints.toSeq)
    angles.exists(_.isLeft)
  }


  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val in = 1 to n map (_ => {
      val x :: y :: Nil = readListInt()
      Paint(x, y)
    })
    println(solution(in) match {
      case true => "YES"
      case false => "NO"
    })
  }

  SetInt(
    """4
      |0 0
      |0 1
      |1 1
      |1 0
    """.stripMargin)
}


//case class Triangle(left: Paint, mid: Paint, right: Paint) {
//  private lazy val a = (left - mid).length
//  private lazy val b = (right - mid).length
//  private lazy val c = (left - right).length
//
//  lazy val isObtuse = {
//    /**/ if (a > b && a > c) a * a > (b * b + c * c)
//    else if (b > a && b > c) b * b > (a * a + c * c)
//    else if (c > a && c > b) c * c > (a * a + b * b)
//    else ???
//  }
//
//  lazy val isRight = {
//    /**/ if (a > b && a > c) a * a == (b * b + c * c)
//    else if (b > a && b > c) b * b == (a * a + c * c)
//    else if (c > a && c > b) c * c == (a * a + b * b)
//    else ???
//  }
//}
//def paintsToTriangle(paints: Paints) = {
//  def impl(paints: Paints, rt: Seq[Triangle]): Seq[Triangle] = {
//  if (paints.size < 3)
//  rt
//  else {
//  val left :: mid :: right :: Nil = paints.take(3)
//  impl(paints.tail, Triangle(left, mid, right) +: rt)
//}
//}
//  val new_paints = paints ++ paints.take(2)
//  impl(new_paints, Nil)
//}
