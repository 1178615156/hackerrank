package worldCode._5

/**
  * Created by yuJieShui on 2016/7/24.
  */
object CamelCase {
  def solution(s: String): Int = s.filter(_.isUpper).size + 1

  def main(args: Array[String]): Unit = {
    println(
      solution(
        io.StdIn.readLine()
      )
    )
  }
}

object StringConstruction {

  //    if (s.isEmpty) {
  //      i
  //    } else if (!p.contains(s.head)) {
  //      solution(s.tail, p append s.head, i + 1)
  //    } else {
  //      def search(start: Int, end: Int): Int = {
  //        if (start == end)
  //          start
  //        else {
  //          val size = start + (end - start) / 2 + (end - start) % 2
  //          if (p.containsSlice(s.take(size))) {
  //            search(size, end)
  //          } else {
  //            search(start, size - 1)
  //          }
  //        }
  //      }
  //      val st = System.currentTimeMillis()
  //      val x = search(1,p.size)
  //      seatchTime = System.currentTimeMillis() - st
  //      solution(s.drop(x), p append s.take(x), i)
  //    }
  def solution(s: Seq[Char]): Int = {
    s.distinct.size
  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val out = 1 to n map { _ =>
      io.StdIn.readLine()
    } map { s => solution(StringBuilder.newBuilder.append(s)) }
    println(
      out mkString "\n"
    )
  }
}