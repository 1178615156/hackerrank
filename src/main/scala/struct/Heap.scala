package struct

import java.util.NoSuchElementException

import scala.util.Random

/**
  * Created by yuJieShui on 2016/8/1.
  */
sealed trait Heap[+T] {
  def isEmpty: Boolean

  def nonEmpty = !isEmpty

  def max: T
}

final case object EmptyHeap extends Heap[Nothing] {
  override def isEmpty: Boolean = true

  override def max: Nothing = Heap.no_max_value()
}

final case class LeafHeap[T](value: T) extends Heap[T] {
  override def isEmpty: Boolean = false

  override def max: T = value
}

final case class BinaryHeap[T](left: Heap[T], right: Heap[T])(implicit val order: Ordering[T]) extends Heap[T] {
  def isEmpty: Boolean = left.isEmpty && right.isEmpty

  override val max: T =
    if (left.isEmpty && right.isEmpty) Heap.no_max_value()
    else if (left.isEmpty) right.max
    else if (right.isEmpty) left.max
    else order.max(left.max, right.max)
}

final object Heap {
  def no_max_value() = throw new NoSuchElementException("no max value")

  def empty[T]: Heap[T] = EmptyHeap

  def cons[T](left: Heap[T], right: Heap[T])(implicit order: Ordering[T]) =
    if (left.isEmpty && right.isEmpty)
      empty[T]
    else
      BinaryHeap(left, right)

  def apply[T](seq: Seq[T])(implicit order: Ordering[T]): Heap[T] = {
    seq match {
      case Seq()         => empty[T]
      case head +: Seq() => LeafHeap(head)
      case other         =>
        val (left, right) = other.splitAt(other.size / 2)
        cons(apply(left), apply(right))
    }
  }


  def dropMax[T](heap: Heap[T])(implicit order: Ordering[T]): Heap[T] = {
    heap match {
      case BinaryHeap(left, right) if left.isEmpty  => cons(left, dropMax(right))
      case BinaryHeap(left, right) if right.isEmpty => cons(dropMax(left), right)
      case BinaryHeap(left, right)                  =>
        order.compare(left.max, right.max) match {
          case e if e < 0  => cons(left, dropMax(right))
          case e if e == 0 => cons(dropMax(left), right)
          case e if e > 0  => cons(dropMax(left), right)
        }
      case LeafHeap(x)                              => EmptyHeap
      case EmptyHeap                                => no_max_value()

    }
  }

  def insert[T](value: T, heap: Heap[T], random: Random = new Random())
               (implicit
                order: Ordering[T]): Heap[T] = {
    heap match {
      case EmptyHeap               => LeafHeap(value)
      case x@LeafHeap(_)           => BinaryHeap(x, LeafHeap(value))
      case BinaryHeap(left, right) =>
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



