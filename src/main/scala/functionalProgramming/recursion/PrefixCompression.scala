package functionalProgramming.recursion

class PrefixCompression {


  def impl(s1: Seq[Char], s2: Seq[Char], same: Seq[Char]): (String, String, String) = {
    if(s1.isEmpty || s2.isEmpty || s1.head != s2.head)
      (same.reverse.mkString(""), s1.mkString(""), s2.mkString(""))
    else
      impl(s1.tail, s2.tail, s1.head +: same)
  }

  def solution(s1: String, s2: String) = {
    impl(s1, s2, Nil)
  }

  def readListInt(): List[Int] = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val s1 = io.StdIn.readLine()
    val s2 = io.StdIn.readLine()
    val (same, s1Result, s2Result) = solution(s1, s2)
    println(
      s"""${same.size} $same
         |${s1Result.size} $s1Result
         |${s2Result.size} $s2Result""".stripMargin)
  }
}
