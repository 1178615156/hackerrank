package algorithms.bitManipulation

import utils.SetInt

/**
  * Created by yuJieShui on 2016/7/12.
  */
object CountGame {
  val bits = 0 to 64 map (BigInt(2) << _)

  object GameUser extends Enumeration {
    val Louise  = Value("Louise")
    val Richard = Value("Richard")
    type GameUser = GameUser.Value

    def nextGameUser(x: GameUser) = x match {
      case Louise  => Richard
      case Richard => Louise
    }

    def beforeGameUser(x: GameUser) = x match {
      case Louise  => Richard
      case Richard => Louise
    }

  }

  import GameUser._




  def solution(n: BigInt, gameUser: GameUser = Louise): GameUser = {
    if (bits.contains(n))
      solution(n / 2, nextGameUser(gameUser))
    else
      bits.filter(_ < n).lastOption match {
        case Some(x) => solution(n - x, nextGameUser(gameUser))
        case None    => beforeGameUser(gameUser)
      }
  }

  def main(args: Array[String]) {
    val n = io.StdIn.readLine().toInt
    val data = 1 to n map {_ => BigInt(io.StdIn.readLine())}
    val out = data map (n => solution(n, Louise).toString) mkString ("\n")
    println(out)
  }

  SetInt(
    """6
      |9007199254740992
      |3876255231698756951
      |2505421038165995835
      |9181662375833297205
      |9876967415056854651
      |2147483648""".stripMargin
  )
}

