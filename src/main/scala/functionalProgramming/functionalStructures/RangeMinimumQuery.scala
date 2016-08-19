package functionalProgramming.functionalStructures

/**
  * Created by yujieshui on 2016/8/19.
  */
object BinaryTree{

  trait Tree[+T] {
    def start: Int

    def end: Int

    def isEmpty: Boolean

    def nonEmpty = !isEmpty

    def min: T
  }

  case object Empty extends Tree[Nothing] {
    override def start: Int = ???

    override def end: Int = ???

    override def min: Nothing = ???

    override def isEmpty: Boolean = true
  }

  case class Leaf[T](value: T,
                     override val start: Int,
                     override val end: Int) extends Tree[T] {

    def right: Tree[T] = Empty

    def left: Tree[T] = Empty

    override def min: T = value

    override def isEmpty: Boolean = false
  }

  case class Binary[T: Ordering](left: Tree[T],
                                 right: Tree[T],
                                 override val start: Int,
                                 override val end: Int) extends Tree[T] {
    override val min: T =
      if (left.nonEmpty && right.nonEmpty) implicitly[Ordering[T]].min(left.min, right.min)
      else if (left.isEmpty && right.nonEmpty) right.min
      else if (left.nonEmpty && right.isEmpty) right.min
      else ???

    override def isEmpty: Boolean = false
  }

  def apply(seq: Seq[Int], start: Int, end: Int): Tree[Int] = {
    if (seq.isEmpty) Empty
    else if (seq.size == 1) Leaf(seq.head, start, end)
    else {
      val mod = seq.size / 2
      val left = seq.take(mod)
      val right = seq.drop(mod)
      Binary(
        apply(left, start, start + mod),
        apply(right, start + mod, end),
        start, end
      )
    }
  }


}
class RangeMinimumQuery {
  import BinaryTree._


  def mkBinaryTree(array: Array[Int]): Tree[Int] = ???

  def rangeTree(start: Int, end: Int)(tree: Tree[Int]): Seq[Tree[Int]] = ???

  def solution(start: Int, end: Int)(tree: Tree[Int]): Int = {
    rangeTree(start, end)(tree).map(_.min).min
  }
}


















