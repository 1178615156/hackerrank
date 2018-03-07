package dataStructures.arrays

object AlgorithmicCrush {

  case class Action(start: Int, end: Int, k: Long)

  def solution(n: Int, actions: Seq[Action]): Long = {
    val array: Array[Long] = Array.fill(n + 3)(0L)
    actions.foreach { case Action(start, end, k) =>
      array(start) = array(start) + k
      array(end+1) = array(end+1) - k
    }
    val (result,_) = array.toSeq.foldLeft((0L, 0L)) {
      case ((max, curr), k) =>
        val new_curr = curr + k
        val new_max = if(new_curr > max) new_curr else max
        new_max -> new_curr
    }
    result
  }

  def readListInt() = scala.io.StdIn.readLine().split(" ").toList.map(_.toLong)

  def main(args: Array[String]): Unit = {
    val n :: m :: Nil = readListInt()
    val actions = 1L to m map (_ => readListInt() match {
      case a :: b :: k :: Nil => Action(a.toInt, b.toInt, k)
    })
    val result = solution(n.toInt, actions)
    println(result)
  }
}
