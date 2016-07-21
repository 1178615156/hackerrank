package functionalProgramming.functionalStructures

/**
  * Created by yujieshui on 2016/7/21.
  */
object OrderExercises {
  type ArrayInt = Seq[Int]
  type SubArrayInt = Seq[Int]
  type Heads = Seq[SubArrayInt]

  case class MaxValue(value: Int, size: Int)

  def heads(list: ArrayInt, k: Int): Heads = {
    list.toStream.take(k).inits.toSeq.reverse.tail
  }


  def max(heads: Heads): MaxValue = {
    val (sum,  size) = (heads.indices zip heads map {
      case (index, subArray) => (subArray.sum, subArray.size)
    }).sorted.last
    MaxValue(sum, size)
  }

  def spiltSubArray(list: ArrayInt,k:Int):Seq[SubArrayInt]={
    val x = list.take(k)
    if (x.size < k) Nil else x +: spiltSubArray(list.tail,k )
  }

  def solution(list: ArrayInt, k: Int): Seq[Int] = {
    1 to k map(i => spiltSubArray(list,i))
    val new_list = list.dropWhile(_ <= 0)
    if (new_list.isEmpty){
      Nil
    }else{
      val new_list_heads = heads(new_list, k)
      val MaxValue(value, size) = max(new_list_heads)
      value +: solution(new_list.drop(size), k)
    }

  }

  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: k ::Nil = readListInt()
    val data = readListInt()
    val out = solution(data,k)
    println(out.mkString("\n"))

  }
}
