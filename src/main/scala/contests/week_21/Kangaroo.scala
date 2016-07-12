package contests.week_21

/**
  * Created by yuJieShui on 2016/6/29.
  */
object Kangaroo {

  case class Kangaroo(location: Int, rate: Int)

  def solution(a: Kangaroo, b: Kangaroo) = {
    // a.location + time * a.rate = time * b.rate + b.location
    // a.location - b.location = time * (b.rate -a.rage)

    val time: Double =
      (a.location - b.location).toDouble / (b.rate - a.rate)
    time >= 0 && time.toInt * (b.rate - a.rate) == a.location - b.location
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val a :: b :: c :: d :: Nil = readListInt()
    val out = solution(
      Kangaroo(a, b),
      Kangaroo(c, d)
    ) match {
      case true  => "YES"
      case false => "NO"
    }
    println(out)
  }
}
