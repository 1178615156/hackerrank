package algorithms.recursion

object ArithmeticExpressions {

  def solution(seq: List[Int], s: String = "", acc: Long = 0): Option[String] = {
    seq match {
      case Nil if (acc % 101) == 0 => Some(s)
      case Nil                     => None

      case head :: tail if s.isEmpty      => solution(tail, head.toString, seq.head)
      case head :: tail if acc % 101 == 0 => Some(s"$s*${seq.mkString("*")}")
      case head :: tail                   =>
        lazy val plus = solution(tail, s"$s+$head", acc + head)
        lazy val minus = solution(tail, s"$s-$head", acc - head)
        lazy val mutlus = solution(tail, s"$s*$head", acc * head)

        /**/ if(plus.nonEmpty) plus
        else if(minus.nonEmpty) minus
        else if(mutlus.nonEmpty) mutlus
        else None
    }
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val data = readListInt()
    val result = solution(data)
    println(result.get)
  }
}
