package functionalProgramming.functionalStructures

/**
  * Created by yujieshui on 2016/7/21.
  */

class OrderExercises {
  type Arr = Seq[Int]

  case class SubArray(start: Int, end: Int, arr: Arr) {
    val size = end - start + 1
    val sum  = arr.sum
  }

  def sum(seq: Arr): Int = seq.sum

  def max(left: SubArray, right: SubArray): SubArray = {
    if(left.sum > right.sum) left
    else if(left.sum == right.sum && left.start < right.start) left
    else if(left.sum == right.sum && left.start == right.start && left.size < right.size) left
    else right
  }

  def list2subArray(seq: Arr,result:Seq[SubArray]): Seq[SubArray] = {
    if (seq.isEmpty) result
    else {
      if (seq.head >0) seq.takeWhile(_ >0)
    }
    ???
  }

  def findMax(seq: Seq[SubArray]): (SubArray, Seq[SubArray]) = ???

  def clear(seq: Seq[SubArray]): Seq[SubArray] = {
    seq.dropWhile(_.sum < 0).reverse.dropWhile(_.sum < 0).reverse
  }

  def impl(_seq: Seq[SubArray], k: Int, result: Seq[SubArray]): Seq[SubArray] = {
    val seq = clear(_seq)
    if(seq.isEmpty || result.size == k) result.reverse
    else {
      val (max, other) = findMax(seq)
      impl(other, k, max +: result)
    }
  }

  def solution(n: Int, k: Int, list: Seq[Int]): Seq[Int] = {
//    val subArrays = list2subArray(list)
//    impl(subArrays, k, Seq()).map(_.sum)
  ???
  }
}

object OrderExercises {
  type ArrayInt = Seq[Int]
  type SubArrayInt = Seq[Int]
  type Heads = Seq[SubArrayInt]

  case class MaxValue(value: Int, size: Int)

  def heads(list: ArrayInt, k: Int): Heads = {
    list.toStream.take(k).inits.toSeq.reverse.tail
  }


  def max(heads: Heads): MaxValue = {
    val (sum, size) = (heads.indices zip heads map {
      case (index, subArray) => (subArray.sum, subArray.size)
    }).sorted.last
    MaxValue(sum, size)
  }

  def spiltSubArray(list: ArrayInt, k: Int): Seq[SubArrayInt] = {
    val x = list.take(k)
    if(x.size < k) Nil else x +: spiltSubArray(list.tail, k)
  }

  def solution(list: ArrayInt, k: Int): Seq[Int] = {
    1 to k map (i => spiltSubArray(list, i))
    val new_list = list.dropWhile(_ <= 0)
    if(new_list.isEmpty) {
      Nil
    } else {
      val new_list_heads = heads(new_list, k)
      val MaxValue(value, size) = max(new_list_heads)
      value +: solution(new_list.drop(size), k)
    }

  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    val n :: k :: Nil = readListInt()
    val data = readListInt()
    val out = solution(data, k)
    println(out.mkString("\n"))

  }
}
