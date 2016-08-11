package algorithms.gameTheory

/**
  * Created by yujieshui on 2016/8/11.
  */
object IntroductionNimGame {

  object Play extends Enumeration {
    val First  = Value("First")
    val Second = Value("Second")
    type Play = Value

    def switchPlay(play: Play) = play match {
      case First  => Second
      case Second => First
    }
  }

  import Play._

  def solution(seq: Seq[Int], play: Play) = {
    if (seq.size == 1) play
    else if(seq.reduceLeft(_ ^ _) > 0) play else switchPlay(play)
  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val out =
      1 to n map (_ => {readListInt();readListInt()}) map (solution(_, Play.First))
    println(out mkString "\n")
  }
}
