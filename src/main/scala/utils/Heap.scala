package utils

import java.util.NoSuchElementException

import scala.util.Random

/**
  * Created by yuJieShui on 2016/8/1.
  */
trait Heap[+T] {
  def isEmpty: Boolean

  def nonEmpty = !isEmpty

  def max: T
}

object Heap {
  def no_max_value() = throw new NoSuchElementException("no max value")

  case object EmptyHeap extends Heap[Nothing] {
    override def isEmpty: Boolean = true

    override def max: Nothing = no_max_value()
  }

  case class Leaf[T](value: T) extends Heap[T] {
    override def isEmpty: Boolean = false

    override def max: T = value
  }

  case class NodeHeap[T](left: Heap[T], right: Heap[T])(implicit val order: Ordering[T]) extends Heap[T] {
    def isEmpty: Boolean = left.isEmpty && right.isEmpty

    override val max: T =
      if (left.isEmpty && right.isEmpty) no_max_value()
      else if (left.isEmpty) right.max
      else if (right.isEmpty) left.max
      else order.max(left.max, right.max)
  }

  def empty[T]: Heap[T] = EmptyHeap

  def cons[T](left: Heap[T], right: Heap[T])(implicit order: Ordering[T]) =
    if (left.isEmpty && right.isEmpty)
      empty[T]
    else
      NodeHeap(left, right)

  def apply[T](seq: Seq[T])(implicit order: Ordering[T]): Heap[T] = {
    seq match {
      case Seq()         => empty[T]
      case head +: Seq() => Leaf(head)
      case other         =>
        val (left, right) = other.splitAt(other.size / 2)
        cons(apply(left), apply(right))
    }
  }


  def dropMax[T](heap: Heap[T])(implicit order: Ordering[T]): Heap[T] = {
    heap match {
      case NodeHeap(left, right) if left.isEmpty  => cons(left, dropMax(right))
      case NodeHeap(left, right) if right.isEmpty => cons(dropMax(left), right)
      case NodeHeap(left, right)                  =>
        order.compare(left.max, right.max) match {
          case e if e < 0  => cons(left, dropMax(right))
          case e if e == 0 => cons(dropMax(left), right)
          case e if e > 0  => cons(dropMax(left), right)
        }
      case Leaf(x)                                => EmptyHeap
      case EmptyHeap                              => no_max_value()

    }
  }

  def insert[T](value: T, heap: Heap[T], random: Random = new Random())
               (implicit
                order: Ordering[T]): Heap[T] = {
    heap match {
      case EmptyHeap             => Leaf(value)
      case x@Leaf(_)             => NodeHeap(x, Leaf(value))
      case NodeHeap(left, right) =>
        if (random.nextInt() % 2 == 0)
          cons(insert(value, left, random), right)
        else
          cons(left, insert(value, right, random))
    }
  }

  def merge[T](main: Heap[T], from: Heap[T])(implicit
                                             order: Ordering[T]): Heap[T] =
    cons(main, from)

}



