package functionalProgramming.functionalStructures

import utils.SetInt

/**
  * Created by yuJieShui on 2016/7/19.
  */
object SubstringSearching {

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: Nil = readListInt()
    val data = 1 to n map { _ =>
      val text = io.StdIn.readLine()
      val pat = io.StdIn.readLine()

      text -> pat
    }
    val out = data.map { case (text, pat) => text.contains(pat)
    }.map {
      case true  => "YES"
      case false => "NO"
    }
    println(out.mkString("\n"))
  }

  SetInt(
    """4
      |abcdef
      |def
      |computer
      |muter
      |stringmatchingmat
      |ingmat
      |videobox
      |videobox
      |
    """.stripMargin)
}
