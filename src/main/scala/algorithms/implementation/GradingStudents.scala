package algorithms.implementation

/**
  * Created by yujieshui on 2017/5/9.
  */
object GradingStudents {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  val multipleByFive = 0 to(100, 5)

  def solution(seq: Seq[Int]) = {
    seq map {
      case e if e < 38   => e
      case e if e >= 100 => e
      case e             =>
        val next = multipleByFive.filter(_ > e).min
        if(next - e < 3) next
        else e
    }
  }

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val data = 1 to n map (_ => readListInt().head)
    val result = solution(data)
    println(result.mkString("\n"))
  }
}
