package functionalProgramming.functionalStructures

import java.io.{BufferedReader, InputStreamReader}

/**
  * Created by yujieshui on 2016/8/19.
  */
object BinaryTree {

  trait Tree[+T] {
    def start: Int

    def end: Int

    def isEmpty: Boolean

    def nonEmpty = !isEmpty

    def min: T

    override def toString: String = s"$start $end $min"
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

object RangeMinimumQuery {

  import BinaryTree._

  def tree2value(tree: Tree[Int]): Seq[Int] = tree match {
    case Empty => Seq()
    case Leaf(value, _, _) => Seq(value)
    case Binary(left, right, _, _) => tree2value(left) ++ tree2value(right)
  }

  def rightTree(start: Int)(tree: Tree[Int]): Seq[Tree[Int]] = tree match {
    case Binary(left, right, binaryStart, end) =>
      if (start < left.end)
        right +: rightTree(start)(left)
      else
        rightTree(start)(right)

    case x@Leaf(value, leafStart, end) =>
      if (leafStart >= start) Seq(x) else Seq()
    case Empty => Seq()
  }

  def leftTree(end: Int)(tree: Tree[Int]): Seq[Tree[Int]] = tree match {
    case Binary(left, right, start, binaryEnd) =>
      if (end  > left.end)
        left +: leftTree(end)(right)
      else
        leftTree(end)(left)
    case x@Leaf(value, start, leafEnd) =>
      if (leafEnd <= end) Seq(x) else Seq()
    case Empty => Seq()
  }

  def solution(start: Int, end: Int)(tree: Tree[Int]): Int = {
    val r = rightTree(start)(tree)
    val l = leftTree(end)(tree)
    val trees = r.filter(_.end < end) ++ l .filter(_.start >= start)

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


















