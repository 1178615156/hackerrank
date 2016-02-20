package algorithms.warmup

import java.io.ByteArrayInputStream

import utils.SetInt

/**
  * Created by yuJieShui on 2016/1/1.
  */
object ArraySum {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var n = sc.nextInt();
    var arr = new Array[Int](n);
    for (arr_i <- 0 to n - 1) {
      arr(arr_i) = sc.nextInt();
    }
    println(arr.sum)
  }
}

object BigIntSum {
  val sc  = new java.util.Scanner(System.in);
  val n   = sc.nextInt();
  val arr = new Array[Int](n);
  for (arr_i <- 0 to n - 1) {
    arr(arr_i) = sc.nextInt();
  }
  val out = arr.foldLeft(0l)(_ + _)
  println(out)

}


object PlusMinus {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val n = sc.nextInt();
    val arr = new Array[Int](n);
    for (arr_i <- 0 to n - 1) {
      arr(arr_i) = sc.nextInt();
    }
    val out = arr.foldLeft((0, 0, 0)) { (l, r) ⇒
      if (r > 0)
        l.copy(_1 = l._1 + 1)
      else if (r == 0)
        l.copy(_2 = l._2 + 1)
      else
        l.copy(_3 = l._3 + 1)

    } match {
      case (positive, zeros, negative) ⇒
        (positive.toDouble / n, zeros.toDouble / n, negative.toDouble / n)
    }
    printf("%f\n", (out._1))
    printf("%f\n", (out._3))
    printf("%f\n", (out._2))
  }
}

object Staircase {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val n = sc.nextInt();
    val out = 1 to n map { i ⇒
      " " * (n - i) + "#" * i
    } mkString "\n"
    println(out)
  }
}

object TimeConversion {
  SetInt.apply(
    """
      |07:05:45PM
    """.stripMargin)

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var time = sc.next();
    val pmRegex = "([0-9][0-9]):([0-9][0-9]):([0-9][0-9])PM".r
    val amRegex = "([0-9][0-9]):([0-9][0-9]):([0-9][0-9])AM".r
    val out = time match {
      case amRegex(h, m, s) ⇒ s"${if (h.toInt == 12) "00" else h}:$m:$s"
      case pmRegex(h, m, s) ⇒ s"${if (h.toInt == 12) h else h.toInt + 12}:$m:$s"
    }
    println(out)
  }
}

object DiaogonalDifference {
  SetInt.apply(
    """
      |3
      |11 2 4
      |4 5 6
      |10 8 -12
    """.stripMargin)

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var n = sc.nextInt();
    var a = Array.ofDim[Int](n, n);
    for (a_i <- 0 to n - 1) {
      for (a_j <- 0 to n - 1) {
        a(a_i)(a_j) = sc.nextInt();
      }
    }
    val b = a.map(_.reverse)
    val index = 0 to n - 1
    val plusDiagonal = index.map(i ⇒ a.apply(i).apply(i)).sum
    val minesDiagonal = index.map(i ⇒ b(i)(i)).sum
    println(math.abs(plusDiagonal - minesDiagonal))
  }
}