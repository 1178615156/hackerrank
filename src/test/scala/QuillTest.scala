import org.scalatest.WordSpec

import scala.annotation.tailrec

/**
  * Created by yujieshui on 2017/6/29.
  */
object Interleave {

  def news[T](l1: List[T], l2: List[T]): List[T] = {
    impl(l1, l2, List.empty[T])
  }

  def impl[T](l1: List[T], l2: List[T], acc: List[T]): List[T] = {
    (l1, l2) match {
      case (Nil, l2)            =>acc.reverse ++ l2
      case (l1, Nil)            =>acc.reverse ++ l1
      case (h1 :: t1, h2 :: t2) => impl(t1, t2, h2 +: h1 +: acc)
    }
  }

  def apply[T](l1: List[T], l2: List[T]): List[T] =
    interleave(l1, l2, List.empty)

  @tailrec
  private[this] def interleave[T](l1: List[T], l2: List[T], acc: List[T]): List[T] =
    (l1, l2) match {
      case (Nil, l2)            => acc ++ l2
      case (l1, Nil)            => acc ++ l1
      case (h1 :: t1, h2 :: t2) => interleave(t1, t2, acc :+ h1 :+ h2)
    }
}

class QuillTest extends WordSpec {

  import org.scalameter._

  val l1 = (1 to 100000).toList
  val l2 = (1 to 100000).toList
  lazy val oldResult = Interleave(l1, l2)
  lazy val newResult = Interleave.news(l1, l2)
  "old" in {

    val time = org.scalameter.config()
      .withWarmer(Warmer.Zero)
      .measure(
        oldResult
      )
    println(time)
  }
  "new" in {
    val time = org.scalameter.config()
      .withWarmer(Warmer.Zero)
      .measure(
        newResult
      )
    println(time)
  }
  "final" in {
    assert(oldResult === newResult)
  }
}




















