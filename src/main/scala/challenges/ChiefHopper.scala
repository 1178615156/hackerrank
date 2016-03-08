package challenges

import utils.SetInt

/**
  * Created by yujieshui on 2016/3/8.
  */
object ChiefHopper {

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def check(list: List[Int], nowEnergy: Long, rt: Boolean = true): Boolean = {
    if (list.isEmpty)
      rt
    else {
      val temp_energy = nowEnergy + nowEnergy - list.head

      if (temp_energy < 0)
        false
      else
        check(list.tail, temp_energy, true)
    }
  }

  def binarySearch(list: List[Int], start: Long, end: Long): Long = {
    end - start match {
      case 0 |1 | 2 =>
        (start.toInt to (Int.MaxValue-1) find (e => check(list, e))).get
      case e =>
        val mid = (start + end) / 2
        if (check(list, mid))
          binarySearch(list, start, mid)
        else
          binarySearch(list, mid + 1, end)
    }
  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val data = readListInt()
    //    val result = jump(data)
    val result = binarySearch(data, 0, data.foldLeft(0l) {
      _ + _
    })
    println(result)
  }

  SetInt(
    """2
      |4 4
    """.stripMargin)
}
