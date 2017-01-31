package functionalProgramming.functionalStructures

/**
  * Created by yujieshui on 2017/1/31.
  */


class JohnAndFences {


  def carve(i: Int, n: Int, value: Int, seq: Seq[Int], reverse: Seq[Int]): Int = {
    def carve_before(n: Int, seq: Seq[Int], result: Int): Int = {
      if(seq.isEmpty) result
      else if(seq.head < n) result
      else carve_before(n, seq.tail, result + n)
    }

    val head = seq.drop(i)
    val tail = reverse.drop(n - i)
    carve_before(value, head, 0) + carve_before(value, tail, 0)
  }

  def carver_opt(seq: Seq[Int], base: Int): Seq[Int] = {
    if(seq.isEmpty) Seq()
    else {
      def spilt[T](e: T, seq: Seq[T], cache: Seq[T], result: Seq[Seq[T]]): Seq[Seq[T]] = {
        if(seq.isEmpty && cache.isEmpty) result
        else if(seq.isEmpty && cache.nonEmpty) cache.reverse +: result
        else if(seq.head == e && cache.isEmpty) spilt(e, seq.tail, cache, result)
        else if(seq.head == e && cache.nonEmpty) spilt(e, seq.tail, Seq(), cache.reverse +: result)
        else spilt(e, seq.tail, seq.head +: cache, result)
      }

      val min = seq.min
      val par = spilt(0, seq.map(_ - min), Seq(), Seq())
      (min + base) * seq.size +: par.flatMap(e => carver_opt(e, min + base))
    }
  }

  def solution(seq: Seq[Int]): Int = {
    carver_opt(seq,0).max
  }
}

object JohnAndFences extends JohnAndFences {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val seq = readListInt()
    val result = solution(seq)
    println(result)
  }
}
