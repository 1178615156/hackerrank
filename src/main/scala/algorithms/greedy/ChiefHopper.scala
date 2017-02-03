package algorithms.greedy

import utils.SetInt

/**
  * Created by yujieshui on 2016/3/8.
  */
object ChiefHopper {


  def check(list: List[Int], nowEnergy: BigInt, max: BigInt): Boolean = {
    lazy val temp_energy = nowEnergy + nowEnergy - list.head
    if(list.isEmpty) true
    else if(temp_energy > max) true
    else if(temp_energy < 0) false
    else check(list.tail, temp_energy, max)
  }


  def search(list: List[Int], start: Long, end: Long, max: BigInt): Long = {
    if(start == end)
      start
    else if(start == end - 1)
      if(check(list, start, max)) start else end
    else {
      val mid = (start + end) / 2
      if(check(list, mid, max))
        search(list, start, mid, max)
      else
        search(list, mid + 1, end, max)
    }
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val data = readListInt()

    val result = search(data, 1, 100000, data.sum)

    println(result)
  }

}
