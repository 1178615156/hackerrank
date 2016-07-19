package algorithms.strings

import scala.annotation.tailrec

import utils.SetInt

/**
  * Created by yuJieShui on 2016/1/1.
  */
object Pangrams {
  SetInt.apply(
    "We promptly judged antique ivory buckles for the next prize    ")

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val txt = sc.nextLine().toLowerCase.toCharArray
    val isPangrams = ('a' to 'z').forall(r => {
      txt.contains(r)
    })

    if (isPangrams)
      println("pangram")
    else
      println("not pangram")
  }
}

object FunnyString {
  SetInt.apply(
    """
      |2
      |acxz
      |bcxz
    """.stripMargin)

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    sc.nextLine()
    val texts = 1 to n map (_ ⇒ sc.nextLine())
    texts.foreach(s ⇒ {
      val l = s.foldLeft((s.head, List[Int]())) { (l, r) ⇒
        (r, math.abs((l._1 - r)) +: l._2)
      }._2
      val r = s.reverse.foldLeft((s.last, List[Int]())) { (l, r) ⇒
        (r, math.abs((l._1 - r)) +: l._2)
      }._2
      val result = l zip r forall (e ⇒ e._1 == e._2)
      if (result)
        println("Funny")
      else
        println("Not Funny")
    })
  }
}

object AlternationCharacters {
  val s: String = 1 to 10000 map (_ ⇒ "A") mkString ""
  SetInt(
    s"""
       |5
       |$s
       |BBBBB
       |ABABABAB
       |BABABA
       |AAABBB
    """.stripMargin)

  @scala.annotation.tailrec
  def run(s: Seq[Char], char: Char, rt: Int): Int = {

    if (s.isEmpty)
      rt
    else if (s.head == char)
      run(s.tail, char, rt + 1)
    else
      run(s.tail, s.head, rt)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    sc.nextLine()
    val texts = 1 to n map (_ ⇒ sc.nextLine())
    val out = texts.par.map(s ⇒ {
      val rt = run(augmentString(s).tail.toList, s.head, 0)
      rt
    }).mkString("\n")
    println(out)
  }
}