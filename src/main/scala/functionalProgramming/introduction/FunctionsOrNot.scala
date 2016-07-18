package functionalProgramming.introduction

/**
  * Created by yujieshui on 2016/7/18.
  */
object FunctionsOrNot {

  def solution(relation:Seq[(Int,Int)])={
    relation.groupBy(_._1).mapValues(_.map(_._2).distinct).forall(_._2.size ==1)
  }
  def main(args: Array[String]): Unit = {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n = readListInt().head
    val data =
    1 to n map {_ =>
      val m = readListInt().head
      1 to m map{_ =>
        val x :: y :: Nil = readListInt()
        x -> y
      }
    }
    val out = data map solution

    println(
      out.map{
        case true => "YES"
        case false => "NO"
      }.mkString("\n")
    )

  }
}
