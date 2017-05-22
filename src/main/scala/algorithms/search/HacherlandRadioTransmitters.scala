package algorithms.search

/**
  * Created by yujieshui on 2017/5/22.
  */
object HacherlandRadioTransmitters {

  def oldGreedy(houses: List[Int], k: Int): List[List[Int]] = houses match {
    case Nil        => Nil
    case a :: Nil   => List(List(a))
    case a :: other =>
      val roof = other.takeWhile(_ - a <= k)
      if(roof.isEmpty) List(a) :: oldGreedy(other, k)
      else {
        val max = roof.max + k
        val current = other.takeWhile(_ <= max)
        (a :: current) :: oldGreedy(other.drop(current.length), k)
      }
  }


  def solution(houses: Seq[Int], k: Int) = {
    oldGreedy(houses.distinct.sorted.toList, k)
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: k :: Nil = readListInt()
    val data = readListInt()

    val result = solution(data, k).length
    println(result)
  }
}
