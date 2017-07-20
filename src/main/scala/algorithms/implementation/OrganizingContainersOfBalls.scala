package algorithms.implementation

/**
  * Created by yujieshui on 2017/5/23.
  */
object OrganizingContainersOfBalls {
  type Contain = Vector[Long]
  type Matrix = Seq[Contain]

  def solution(matrix: Matrix): Boolean = {
    matrix.map(_.sum).sorted == matrix.transpose.map(_.sum).sorted
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val q :: Nil = readListInt()
    val data =
      1 to q map { _ =>
        val n :: Nil = readListInt()
        1 to n map (_ => io.StdIn.readLine().split(" ").toVector.map(_.toLong))
      }

    val result = data.map(solution).map {
      case true  => "Possible"
      case false => "Impossible"
    }
    println(result.mkString("\n"))
  }
}
