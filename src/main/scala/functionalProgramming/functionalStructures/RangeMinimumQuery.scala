package functionalProgramming.functionalStructures

import java.io.{BufferedReader, InputStreamReader}



object RangeMinimumQuery {

  import functionalProgramming.BinaryTree._




  def solution(start: Int, end: Int)(tree: Tree[Int]): Int = {
    //    val r = rightTree(start)(tree)
    //    val l = leftTree(end)(tree)
    //    r.filter(_.end < end) ++ l.filter(_.start >= start)
    val trees = subArrTree(start, end)(tree)
    trees.map(_.min).min
  }

  def main(args: Array[String]): Unit = {
    val bi = new BufferedReader(new InputStreamReader(System.in))
    def readListInt() = bi.readLine().split(" ").toList.map(_.toInt)
    val m :: n :: Nil = readListInt()
    val array = bi.readLine().split(" ").map(_.toInt).toSeq
    val tree = apply(array, 0, m)
    val in = 1 to n map (_ => readListInt())
    val result = in map { l =>
      solution(l(0), l(1) + 1)(tree)
    }
    println(
      result.mkString("\n")
    )
  }
}


















