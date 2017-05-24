package algorithms.implementation

/**
  * Created by yujieshui on 2017/5/24.
  */
object QyeensAttackII {

  case class Position(row: Int, column: Int)

  def solution(n: Int, q: Position, ks: Set[Position]) = {

    val a = q.row to(1, -1) map (row => Position(row, q.column)) takeWhile (!ks.contains(_))
    val b = q.row to n map (row => Position(row, q.column)) takeWhile (!ks.contains(_))

    val c = q.column to(1, -1) map (column => Position(q.row, column)) takeWhile (!ks.contains(_))
    val d = q.column to n map (column => Position(q.row, column)) takeWhile (!ks.contains(_))

    val e = (q.row to n) zip (q.column to n) map { case (row, col) => Position(row, col) } takeWhile (!ks.contains(_))
    val f = (q.row to(1, -1)) zip (q.column to(1, -1)) map { case (row, col) => Position(row, col) } takeWhile (!ks.contains(_))

    val g = (q.row to n) zip (q.column to(1, -1)) map { case (row, col) => Position(row, col) } takeWhile (!ks.contains(_))
    val h = (q.row to(1, -1)) zip (q.column to n) map { case (row, col) => Position(row, col) } takeWhile (!ks.contains(_))

    List(a, b, c, d, e, f, g, h).flatten.filter(_ != q).size
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: k :: Nil = readListInt()
    val qr :: qc :: Nil = readListInt()

    val ks = 1 to k map (_ => readListInt()) map { case r :: c :: Nil => Position(r, c) }
    val result = solution(n, Position(qr, qc), ks.toSet)
    println(result)
  }
}
