package functionalProgramming.recursion

import utils.SetInt

/**
  * Created by yujieshui on 2016/7/19.
  */
object StringCompression {
  def impl(s:Seq[Char],rt:Seq[String]):String={
    if (s.isEmpty){
      rt.reverse.foldLeft(new StringBuilder())(_ ++= _).toString()
    }else{
      val x = s.takeWhile(_ == s.head)
      impl(s.drop(x.size),(if (x.size >1) s.head.toString + x.size else s.head.toString) +: rt)
    }
  }
  def solution(s:String)={
    impl(s,Seq())
  }

  def main(args: Array[String]): Unit = {
    val s = io.StdIn.readLine()
    println(
      solution(s)
    )
  }



  SetInt(
    """aaabaaaaccaaaaba
      |
    """.stripMargin)
}
