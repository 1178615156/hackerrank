package functionalProgramming.recursion

object StringReductions {
  def solution(head: Seq[Char], tail: List[Char]): String = {
    tail match {
      case Nil                            => head.reverse.mkString("")
      case a :: other if head.contains(a) => solution(head, other)
      case a :: other                     => solution(a +: head, other)
    }
  }

  def main(args: Array[String]): Unit = {
    val s = io.StdIn.readLine()
    val result = solution(Nil, s.toList)
    println(result)
  }
}
