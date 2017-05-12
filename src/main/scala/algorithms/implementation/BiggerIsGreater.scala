package algorithms.implementation


/**
  * Created by yujieshui on 2017/5/11.
  */
object BiggerIsGreater {

  import scala.util.Try
  import scala.annotation.tailrec

  @tailrec def impl(s: List[Char], tails: List[Char] = Nil): List[Char] = s match {
    case Nil                      => tails
    case a :: Nil                 => a :: tails
    case a :: b :: other if a > b =>
      val (l, min) = (b :: tails).foldLeft((List[Char](), a)) {
        case ((list, min), element) =>
          if(element > b && element < min)
            (min :: list) -> element
          else
            (element :: list) -> min
      }
      other.reverse ::: (min :: l.sorted)
    case a :: b :: other          =>
      impl(b :: other, a :: tails)
  }

  @inline def solution(s: List[Char]) = {
    val result = impl(s.reverse)
    if(result == s) None else Some(result)
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readLine().toInt
    val data = (1 to n) map (_ => io.StdIn.readLine())


    val result = data map (e => solution(e.toList) match {
      case Some(x) => (x.mkString(""))
      case None    => ("no answer")
    })
    println(result.mkString("\n"))
  }

}
