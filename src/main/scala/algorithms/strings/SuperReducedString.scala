package algorithms.strings

/**
  * Created by yujieshui on 2017/3/29.
  */
object SuperReducedString {
  def impl(l: List[Char]): List[Char] = {
    l match {
      case Nil                       => Nil
      case a :: Nil                  => a :: Nil
      case a :: b :: other if a == b => impl(other)
      case a :: b :: other if a != b =>
        val x = a :: impl(b :: other)
        if(x.size == l.size) x else impl(x)
    }
  }

  def solution(s: String): String =
    impl(s.toList).mkString("")

  def main(args: Array[String]): Unit = {
    val in = io.StdIn.readLine()
    val result = solution(in)
    val out =if(result.isEmpty)
      "Empty String"
    else
      result

    println(out )
  }
}
