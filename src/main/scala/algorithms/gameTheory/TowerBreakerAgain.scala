package algorithms.gameTheory

/**
  * Created by yujieshui on 2016/8/12.
  */
class TowerBreakerAgain {

  def solution(seq: Seq[Int]) = {
    if (seq.count(_ > 1) % 2 == 1) "1" else "2"
  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val in = 1 to n map (_ => {readListInt();readListInt()})

    val result = in map solution

    println(result mkString "\n")
  }
}
