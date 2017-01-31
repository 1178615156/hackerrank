package functionalProgramming.functionalStructures

/**
  * Created by yujieshui on 2017/1/26.
  */


class PrisonTransport {


  def to_group(i: Int, map: Map[Int, Seq[Int]], result: Set[Int]): Set[Int] = {
    val seq = map(i)
    seq.filterNot(result.contains(_)).foldLeft(result ++ seq.toSet) {
      case (result, element) =>
        to_group(element, map, result)
    }
  }

  def spilt(n: Int, map: Map[Int, Seq[Int]], result: Set[Set[Int]], cache_value: Set[Int] = Set()): Set[Set[Int]] = {
    if(n <= 0) result
    else if(cache_value.contains(n)) spilt(n - 1, map, result, cache_value)
    else if(!map.contains(n)) spilt(n - 1, map, result + Set(n), cache_value + n)
    else {
      val group = to_group(n, map, Set())
      spilt(n - 1, map, result + group, cache_value ++ group)
    }
  }

  def solution(n: Int, seq: Seq[(Int, Int)]) = {
    val reverse = seq map { case (a, b) => (b, a) }
    val map = (seq ++ reverse).groupBy(_._1).mapValues(_.map(_._2))
    val result = spilt(n, map, Set()).toList

      .map(_.size)
      .map(e => math.ceil(math.sqrt(e)).toInt)
    result.sum
  }
}

object PrisonTransport extends PrisonTransport {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val m :: Nil = readListInt()
    val data = 1 to m map { _ =>
      val a :: b :: Nil = readListInt()
      a -> b
    }

    val result = solution(n, data)
    println(result)
  }
}