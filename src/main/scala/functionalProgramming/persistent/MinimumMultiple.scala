package functionalProgramming.persistent

import java.io.{BufferedReader, InputStreamReader}

import utils.SetInt

/**
  * Created by yujieshui on 2016/8/29.
  */
object MinimumMultiple {
  type Num = BigInt

  def gcd(l: Num, r: Num): Num =
    if (l % r == 0) r else gcd(r, l % r)

  def minimumMultiple(seq: Seq[Long]): Num =
    seq.foldLeft(BigInt(1)) { (l, r) => (l * r) / gcd(l, r) }

  type Arr = Seq[Long]

  def mkArr(seq: Seq[Long]): Arr = {
    seq
  }

  def queryArr(query: Query)(arr: Arr): BigInt = {
    minimumMultiple(arr.slice(query.start, query.end + 1))
  }


  def updateArr(update: Update)(arr: Arr): Arr = {
    arr.updated(update.index, arr(update.index) * update.value)
  }

  sealed trait Action

  case class Query(start: Int, end: Int) extends Action

  case class Update(index: Int, value: Int) extends Action

  def solution(actions: Seq[Action], arr: Arr)(rt: Seq[Num]): Seq[Num] = {
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

  def main(args: Array[String]): Unit = {
    val bi = new BufferedReader(new InputStreamReader(System.in))
    def readListInt() = bi.readLine().split(" ").toSeq.map(_.toLong)

    val n +: Seq() = readListInt()
    val initArr = readListInt()
    val m +: Seq() = readListInt()
    val actions = 1L to m map { _ =>
      bi.readLine().split(" ").toList match {
        case "Q" :: start :: end :: Nil => Query(start.toInt, end.toInt)
        case "U" :: index :: value :: Nil => Update(index.toInt, value.toInt)
      }
    }


    val out = solution(actions, mkArr(initArr))(Seq()) map (_ % (10e9 + 7).toInt) mkString "\n"
    println(out)
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
