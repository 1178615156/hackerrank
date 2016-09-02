package functionalProgramming.persistent

import java.io.{BufferedReader, InputStreamReader}

//import functionFractal.validbst.BinaryTree
import utils.SetInt

import scala.annotation.tailrec
import functionalProgramming.BinaryTree

/**
  * Created by yujieshui on 2016/8/29.
  */
case class CompeteTime() {
  var t = 0L

  def time[T](f: => T) = {
    val st = System.currentTimeMillis()
    val r = f
    val et = System.currentTimeMillis()
    t += (et - st)
    r
  }

  override def toString: String = t.toString
}

sealed trait Action

final case class Query(start: Int, end: Int) extends Action

final case class Update(index: Int, value: Int) extends Action


trait MinimumMultiple {
  final type Num = BigInt
  final val M = BigInt(1000000007L)

  @tailrec
  final def gcd(l: Num, r: Num): Num = if (l % r == 0) r else gcd(r, l % r)

  final def minimumMultiple(seq: Seq[Num]): Num = seq.foldLeft(BigInt(1)) { (l, r) => (l * r) / gcd(l, r) }

  type Arr

  def mkArr(seq: Seq[Long]): Arr

  def queryArr(query: Query)(arr: Arr): Num

  def updateArr(update: Update)(arr: Arr): Arr

  @tailrec
  final def solution(actions: Seq[Action], arr: Arr)(rt: Seq[Num]): Seq[Num] = {
    if (actions.isEmpty)
      rt.reverse
    else {
      actions.head match {
        case q: Query =>
          solution(actions.tail, arr)(queryArr(q)(arr) +: rt)
        case u: Update =>
          solution(actions.tail, updateArr(u)(arr))(rt)
      }
    }
  }

  type NextLine = () => String

  def read(nextLine: NextLine) = {
    def readListInt() = nextLine().split(" ").toSeq.map(_.toLong)

    val n +: Seq() = readListInt()
    val initArr = readListInt()
    val m +: Seq() = readListInt()
    val actions = 1L to m map { _ =>
      nextLine().split(" ").toList match {
        case "Q" :: start :: end :: Nil => Query(start.toInt, end.toInt)
        case "U" :: index :: value :: Nil => Update(index.toInt, value.toInt)
      }
    }

    (initArr, actions)
  }
}

object MinimumMultiple2 extends MinimumMultiple {

  import BinaryTree._

  final override type Arr = Tree[BigInt]
  implicit final val minLong = new Min[BigInt] {
    override def min(l: BigInt, r: BigInt): BigInt = (l * r) / gcd(l, r)
  }

  override def mkArr(seq: Seq[Long]): Arr = {

    apply(seq.map(BigInt.apply), 0, seq.size)
  }

  override def queryArr(query: Query)(arr: Arr): Num = {
    val _arr = subArrTree(query.start, query.end + 1)(arr).map(_.min)
    minimumMultiple(_arr)
  }

  override def updateArr(update: Update)(arr: Arr): Arr = {
    updateTree(update.index, (_: BigInt) * update.value)(arr)
  }



}

object MinimumMultiple extends MinimumMultiple {
  final override type Arr = Array[Long]


  final def mkArr(seq: Seq[Long]): Arr = {
    seq.toArray
  }

  val minimumMultipleTime = CompeteTime()

  def queryArr(query: Query)(arr: Arr): Num = {
    minimumMultipleTime.time {
      val subArr = (query.start to query.end map arr.apply filter (_ > 1)).distinct
      minimumMultiple(subArr.map(e => BigInt(e)))
    }
  }


  def updateArr(update: Update)(arr: Arr): Arr = {
    arr.updated(update.index, arr(update.index) * update.value)
  }




  def main(args: Array[String]): Unit = {
    val bi = new BufferedReader(new InputStreamReader(System.in))
    val (initArr, actions) = read(() => bi.readLine())

    val out = solution(actions, mkArr(initArr))(Seq()) map (_ % M)
    println(out mkString "\n")
  }


  SetInt(
    """5
      |2 5 6 1 9
      |7
      |Q 0 4
      |U 1 2
      |Q 0 2
      |Q 3 4
      |Q 2 4
      |U 3 8
      |Q 2 3
      |
    """.stripMargin)
}
