package functionalProgramming.ad_hoc

import scala.collection.immutable.IndexedSeq

/**
  * Created by yujieshui on 2017/1/5.
  */
class Mangoes {

  def cost(friendSize: Long,
           appetiteList: Seq[Long],
           happyList: Seq[Long]): Seq[Long] = {
    appetiteList zip happyList map {
      case (appetite, happy) =>
        appetite + (friendSize - 1) * happy
    }
  }

  def canInvited(friendSize: Int, mongoSize: Long,
                 appetite: Seq[Long],
                 happy: Seq[Long]): Boolean = {
    val friends = cost(friendSize, appetite, happy).sorted.take(friendSize)
    friends.sum <= mongoSize
  }

  def solution(totalFriendSize: Int, mongoSize: Long,
               appetite: Seq[Long],
               happy: Seq[Long]): Int = {

    def binarySearch(start: Int, end: Int): Int = {
      val mid = (start + end) / 2

      if(mid == start)
        if(canInvited(end, mongoSize, appetite, happy)) end else start
      else if(canInvited(mid, mongoSize, appetite, happy))
        binarySearch(mid, end)
      else
        binarySearch(start, mid)

    }


    binarySearch(0, totalFriendSize)
  }
}

object Mangoes extends Mangoes {
  def readListInt(): List[Long] = io.StdIn.readLine().split(" ").toList.map(_.toLong)

  def main(args: Array[String]): Unit = {
    val n :: m :: Nil = readListInt()
    val appetite = readListInt()
    val happy = readListInt()
    val result = solution(
      n.toInt, m, appetite, happy
    )
    println(result)
  }
}