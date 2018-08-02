package algorithms.implementation

object ClimbingTheLeaderboard {

  import scala.collection.Searching._

  def solution(scores: Seq[Int], alice: Seq[Int]): Seq[Int] = {
    val scoresSord = scores.distinct.reverse
    val len = scoresSord.length
    alice.map { e =>
      scoresSord.search(e) match {
        case Found(x)          => len - x - 1
        case InsertionPoint(x) => len - x + 1
      }
    }
  }

  def readListInt() = io.StdIn.readLine().split(" ").toSeq.map(_.toInt)

  def main(args: Array[String]): Unit = {
    readListInt()
    val scores = readListInt()
    readListInt()
    val alice = readListInt()
    val result = solution(scores, alice)
    println(result.mkString("\n"))
  }
}
