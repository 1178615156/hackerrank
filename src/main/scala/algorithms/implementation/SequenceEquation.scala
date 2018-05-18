package algorithms.implementation

object SequenceEquation {
  def solution(seq: Seq[Int]) = {
    val map = (seq zip seq.indices.map(_ + 1)).toMap
    1 to seq.length map { x =>
      map(map(x))
    }
  }

  def readListInt() = scala.io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n = readListInt()
    val list = readListInt()
    solution(list).foreach(println)
  }
}
