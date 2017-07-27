package functionalProgramming.ad_hoc

import scala.collection.immutable.IndexedSeq


/**
  * Created by yujieshui on 2017/1/6.
  */

object GameOfKyles {
  type Pin = Int

  def downPinAllPossible(int: Int): Seq[(Pin, Pin)] = {
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

  type Result = Map[Set[Int], Boolean]
  var result: Result = Map()


  def seq2set(seq: Seq[Int]): Set[Pin] = {
    seq.groupBy(e => e).mapValues(_.size).collect {
      case (k, size) if size % 2 != 0 => k
    }.toSet
  }

  def playGame(set: Set[Int]): Boolean = {
    val dropReplace = set
    if(set.isEmpty) false
    else if(set.size == 1) true
    else if(result.contains(set)) result(set)
    else {
      val list = set.map { element => element -> (dropReplace - element) }.toSeq
      val allStatus = list.flatMap { case (pin, other) =>
        downPinAllPossible(pin).map {
          case (0, 0) => other
          case (0, b) => if(other.contains(b)) other - b else other + b
          case (a, 0) => if(other.contains(a)) other - a else other + a
          case (a, b) =>
            val aa = if(other.contains(a)) other - a else other + a
            val bb = if(aa.contains(b)) aa - b else aa + b
            bb
        }
      }

      val playResult = allStatus.sortBy(_.size).exists { e => !playGame(e)}
      result = result + (set -> playResult)
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


  def readListInt(): List[Int] = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val total :: Nil = readListInt()
    val inData = 1 to total map { _ =>
      readListInt()
      io.StdIn.readLine()
    }
    val data = inData.map(line2Seq)

    val out = data.map(e => playGame(seq2set(e))).map {
      case true  => "WIN"
      case false => "LOSE"
    }.mkString("\n")
    println(out)
  }
}