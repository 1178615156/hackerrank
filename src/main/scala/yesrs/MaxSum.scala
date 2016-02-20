package yesrs

import utils.SetInt

/**
  * Created by yuJieShui on 2016/1/30.
  */
object MaxSum {


  def sp(l: List[Long], last: Long, now: List[Long], rt: List[List[Long]]): List[List[Long]] = {
    if (l.isEmpty)
      now +: rt
    else if (l.head == 0)
      sp(l.tail, l.head, Nil, now +: rt)
    else if (l.head > 0 && last > 0 || l.head < 0 && last < 0)
      sp(l.tail, l.head, l.head +: now, rt)
    else
      sp(l.tail, l.head, l.head +: Nil, now +: rt)
  }


  def main(args: Array[String]) {

    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val data = 1 to n map (e => sc.nextLong())

    val out =
      sp(data.toList, data.head, Nil, Nil).filter(_.nonEmpty).map(_.sum).filter(_ > 0)

    val max =
      if (out.isEmpty) 0 else out.max

    println(max)
  }


  SetInt(
    """
    """.stripMargin)
}
