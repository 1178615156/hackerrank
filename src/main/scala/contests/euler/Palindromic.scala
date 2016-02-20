package contests.euler

import scala.collection.immutable.IndexedSeq

/**
  * Created by yuJieShui on 2016/1/1.
  */
object Palindromic {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val t = sc.nextInt()
    val data: IndexedSeq[(Int, Int)] = 1 to t map (_ ⇒ sc.nextInt() → sc.nextInt())
    data map {
      case (num,slip)⇒
        num
    }
  }
}
