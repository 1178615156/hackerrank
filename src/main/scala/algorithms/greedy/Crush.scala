package algorithms.greedy

import utils.SetInt

/**
  * Created by yuJieShui on 2016/2/28.
  */
object Crush {
  def readInts() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  case class PlusInterval(begin: Int, end: Int, value: Int)


  def sum(list: Seq[PlusInterval], rt: Int): Int = {
    if (list.isEmpty) {
      rt
    } else {
      val now = list.head
      def takeEndBefore(list: Seq[PlusInterval], rt: List[PlusInterval]): List[PlusInterval] = {
        if (list.isEmpty || list.head.begin > now.end)
          rt
        else
          takeEndBefore(list.tail, list.head +: rt)
      }
      val maxOfNow = takeEndBefore(list.tail, Nil).foldLeft(0) {
        _ + _.value
      } + now.value

      sum(list.tail, if (maxOfNow > rt) maxOfNow else rt)
    }


  }

  def main(args: Array[String]) {
    val n :: m :: Nil = readInts()
    val plusIntervalData = 1 to m map (_ => {
      val begin :: end :: value :: Nil = readInts()
      PlusInterval(begin, end, value)
    })
    println(sum(
      plusIntervalData.sortBy(_.begin),
      Int.MinValue
    ))
  }

  SetInt(
    s"""5 3
        |1 2 100
        |2 5 100
        |3 4 100
     """.stripMargin)
}
