package functionalProgramming.ad_hoc

/**
  * Created by yujieshui on 2017/1/2.
  */
trait KunduAndBubbleWrap {

  def total(n: Int, m: Int): Int = n * m

  def solution(n: Int, m: Int): Double = {
    val total = this.total(n, m)

    total * (1 to total map (1.0 / _)).sum
  }

}
object KunduAndBubbleWrap extends KunduAndBubbleWrap{
  def main(args: Array[String]): Unit = {
    def readListInt(): List[Int] = io.StdIn.readLine().split(" ").toList.map(_.toInt)
      val n :: m :: Nil = readListInt()
    println(solution(n,m))
  }
}