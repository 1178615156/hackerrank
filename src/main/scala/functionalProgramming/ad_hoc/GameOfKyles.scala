package functionalProgramming.ad_hoc

import scala.collection.immutable.IndexedSeq


/**
  * Created by yujieshui on 2017/1/6.
  */

class GameOfKyles {
  type Pin = Int

  def down(int: Int): Seq[(Pin, Pin)] = {
    if(int == 0) Seq()
    else if(int == 1) Seq((0, 0))
    else if(int == 2) Seq((0, 0), (0, 1))
    else {
      val listByOne = 0 until int
      val a = listByOne zip listByOne.reverse
      val listByTwo = 0 until (int - 1)
      val b = listByTwo zip listByTwo.reverse
      a.take(int / 2 + 1) ++ b.take(int / 2)
    }
  }

  type Result = Map[Seq[Int], Boolean]
  var result: Result = Map()

  def enumAll(seq: Seq[Int]): Boolean = {
    val dropReplace = seq.groupBy(e => e).collect {
      case (k, v) if v.size % 2 != 0 => k
    }.toSeq.sorted
    dropReplace.toList match {
      case Nil                     => false
      case a :: Nil                => if(a == 0) false else true
      case a :: b :: Nil if a == b => false
      case e if result.contains(e) => result(e)
      case _ =>

        val list: Seq[(Pin, Seq[Pin])] = dropReplace.indices.map { index =>
          (dropReplace(index), (dropReplace.take(index) ++ dropReplace.drop(index + 1)))
        }
        val allStatus =
          list.flatMap { case (pin, other) =>
            down(pin).map { case (a, b) =>
              Seq(a, b).filter(_ != 0) ++ other

            }
          }
        val playResult =
          allStatus.exists(e => {
            !enumAll(e)
          })
        if(!result.contains(seq))
          result = result ++ Seq(
            (seq, playResult),
            dropReplace -> playResult
          )
        playResult
    }
  }

  def line2Seq(s: String) = {
    s
      .replaceAll("X+", "X")
      .split("X").toSeq
      .filter(_.nonEmpty)
      .map(_.size)
  }

}

object GameOfKyles extends GameOfKyles {
  def readListInt(): List[Int] = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val total :: Nil = readListInt()
    val inData = 1 to total map { _ =>
      readListInt()
      io.StdIn.readLine()
    }
    val data = inData.map(line2Seq)

    val out = data.map(e => enumAll(e)).map {
      case true  => "WIN"
      case false => "LOSE"
    }.mkString("\n")
    println(out)
  }
}