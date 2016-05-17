package algorithms.implementation

/**
  * Created by yuJieShui on 2016/5/7.
  */
object Encryption {

  case class Size(rows: Int, column: Int) {
    def area = rows * column
  }

  def countSize(s: String): Size = {
    val l = s.size
    val floor = math.floor(math.sqrt(l)).toInt
    val ceil = math.ceil(math.sqrt(l)).toInt
    val allSize =
      floor to ceil flatMap (column =>
        floor to column map (rows =>
          Size(rows, column)))

    allSize filter (size => size.rows * size.column >= l) minBy (_.area)
  }

  def encryption(s: String): String = {
    val size@Size(rows, column) = countSize(s)
    (s.toList ++ " " * (size.area - s.size))
      .grouped(column).toList.transpose.map(_.mkString("").trim).mkString(" ")
  }

  def main(args: Array[String]) {
    val in = io.StdIn.readLine()
    val result = encryption(in)
    println(result)
  }
}
