package contests.lambdaCalculiMarch2016

import utils.SetInt

import scala.collection.immutable.IndexedSeq

/**
  * Created by yuJieShui on 2016/3/25.
  */
object FunctionOrNot {

  case class KeyValue(x: Int, y: Int)

  def checkIsFunction(keyValues: Seq[KeyValue]) = {
    keyValues.map(_.x).distinct.size == keyValues.size
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val testNum :: Nil = readListInt()
    val in: IndexedSeq[IndexedSeq[KeyValue]] = 1 to testNum map { _ =>
      val size :: Nil = readListInt()
      1 to size map { _ =>

        val x :: y :: Nil = readListInt()
        KeyValue(x, y)
      }
    }
    val result =
      in map checkIsFunction map {
        case true => "YES"
        case false => "NO"
      }

    println(result.mkString("\n"))
  }

  SetInt(
    """2
      |3
      |1 1
      |2 2
      |3 3
      |4
      |1 2
      |2 4
      |3 6
      |4 8
      |
    """.stripMargin)
}
