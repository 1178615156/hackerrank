package algorithms.gameTheory
import utils.SetInt

/**
  * Created by yujieshui on 2016/10/5.
  */
class FunGame {

  object Play extends Enumeration {
    val First  = Value("First")
    val Second = Value("Second")
    val Tie    = Value("Tie")
    type Play = Value

    def switchPlay(play: Play) = play match {
      case First  => Second
      case Second => First
    }
  }

  import Play._

  case class Entity(a: Int, b: Int,i:Int){
    override def toString: String = s"a=$a,b=$b"
  }

  case class Result(firstNum: Int, secondNum: Int){
    override def toString: String = s"a=$firstNum,b=$secondNum"
  }

  def impl(play: Play, result: Result, list: Seq[Entity]): Seq[Result] = {
    if (list.isEmpty) Seq(result)
    else {
      import result.firstNum
      import result.secondNum
      val nextPlay = switchPlay(play)
      val next = list.map {
        case x@Entity(a, b,_ ) if play == First  =>
          impl(nextPlay, Result(firstNum + a, secondNum), list filterNot (_ ==  x))
        case x@Entity(a, b,_ ) if play == Second =>
          impl(nextPlay, Result(firstNum, secondNum + b), list filterNot (_ ==  x))
      }
     val r = next.map(_.maxBy {
        case Result(firstNum, secondNum) if nextPlay == First  => firstNum - secondNum
        case Result(firstNum, secondNum) if nextPlay == Second => secondNum - firstNum
      })
      r
    }
  }

  def solution( list: Seq[Entity]): Play = {
    val l = impl(First, Result(0, 0), list.toSeq)
    val result =l.maxBy{case Result(firstNum,secondNum) => firstNum - secondNum}
    if (result.firstNum > result.secondNum) First
    else if (result.firstNum== result.secondNum) Tie
    else Second

  }

}

object FunGame extends FunGame{

  def main(args: Array[String]): Unit = {
    var i = 0

    def get_index = {
      i += 1
      i
    }

    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val in =
      1 to n map {i =>
        readListInt()
        readListInt() zip readListInt() map{case (a,b) => Entity(a,b,get_index)}}

    val out = in map (e=>solution(e))

    println(
      out mkString "\n"
    )
  }
  SetInt(
    """3
      |3
      |1 3 4
      |5 3 1
      |2
      |1 1
      |1 1
      |2
      |2 2
      |3 3
      |
    """.stripMargin)
}
