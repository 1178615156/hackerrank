package functionFractal

/**
  * Created by yuJieShui on 2016/1/10.
  */
object ConvexHull extends App {

  case class Point(xx: Int, yy: Int)

  case class Line(head: Point, tail: Point)

  case class Triangle(a: Point, b: Point, c: Point)

  val sc    = new java.util.Scanner(System.in);
  val n     = sc.nextInt()
  val datas = 1 to n map (_ ⇒
    Point(sc.nextInt(), sc.nextInt())
    )

//  scala.util.control.TailCalls.TailRec
}
