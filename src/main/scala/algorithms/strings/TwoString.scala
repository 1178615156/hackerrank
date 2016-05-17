package algorithms.strings

/**
  * Created by yuJieShui on 2016/5/8.
  */
object TwoString {
  def hasPublicSubString(one: String, two: String): Boolean = {
    (one.toSet intersect two.toSet).nonEmpty
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val in = 1 to n map (_ => io.StdIn.readLine() -> io.StdIn.readLine())

    val result =
      in map{case (one,two) => hasPublicSubString(one,two)} map {
        case true  => "YES"
        case false => "NO"
      }
    println(result mkString "\n")
  }
}
