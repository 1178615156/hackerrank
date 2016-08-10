package algorithms.gameTheory

/**
  * Created by yujieshui on 2016/8/10.
  */
object TowerBreakers {

  object Play extends Enumeration {
    val First  = Value("1")
    val Second = Value("2")
    type Play = Value

    def switchPlay(play: Play) = play match {
      case First  => Second
      case Second => First
    }
  }

  import Play._

  type Tower = Int
  type Towers = Seq[Tower]

  def solution(tower_size: Int, height: Int, play: Play) :Play= {
    if (height == 1) switchPlay(play)
    else if (tower_size % 2 == 1) play
    else if (tower_size % 2 == 0) switchPlay(play)
    else ???
  }


  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val out =
    1 to n map (_ => readListInt()) map (l => solution(l(0), l(1), Play.First))

    println(out.mkString("\n"))
  }
}
