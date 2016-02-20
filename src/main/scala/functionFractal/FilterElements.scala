package functionFractal

import utils.SetInt

/**
  * Created by yuJieShui on 2016/1/9.
  */
object FilterElements extends App {
  SetInt(
    """3
      |9 2
      |4 5 2 5 4 3 1 3 4
      |9 4
      |4 5 2 5 4 3 1 3 4
      |10 2
      |5 4 3 2 1 1 2 3 4 5
    """.stripMargin)

  case class Data(m: Int, k: Int, element: List[Int])

  val sc    = new java.util.Scanner(System.in);
  val n     = sc.nextInt()
  val datas = (1 to n) map (_ ⇒ {
    val m = sc.nextInt()
    val k = sc.nextInt()

    Data(m, k, (1 to m) map (_ ⇒ sc.nextInt()) toList)
  })
  val out   = datas.map(data ⇒ {
    val rt = ((data.element.indices zip data.element
      groupBy (_._2)).values
      filter (_.size >= data.k)
      map (_.head)
      )
    if (rt.isEmpty)
      List(-1)
    else
      rt.toList.sortBy(_._1).map(_._2)
  })
  println(out.map(_.mkString(" ")).mkString("\n")
  )
}

object SuperDigit extends App {
  SetInt(
    """1481111111111111111111111111111111111111111111 3
    """.stripMargin
  )

  val sc = new java.util.Scanner(System.in);
  val in = sc.nextLine().split(" ")
  val n  = in.head
  val k  = in.tail.head.toInt

  println(
    superDigit(
      superDigit(n) * k
    )
  )

  def superDigit(n: String): String =
    if (n.length <= 1)
      n
    else
      superDigit(
        n.map(_.toString.toLong).sum.toString
      )

}
