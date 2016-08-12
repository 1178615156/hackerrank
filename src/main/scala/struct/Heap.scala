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

  def depth: Int

}

final case object EmptyHeap extends Heap[Nothing] {
  override val isEmpty: Boolean = true

  override def max: Nothing = Heap.no_max_value()

  override val depth: Int = 0

}

final case class LeafHeap[T](value: T) extends Heap[T] {
  override val isEmpty: Boolean = false

  override val max: T = value

  override val depth: Int = 1

}

final case class BinaryHeap[T](left: Heap[T], right: Heap[T])(implicit val order: Ordering[T]) extends Heap[T] {
  override val isEmpty: Boolean = false

  override val max: T =
    if (left.isEmpty && right.isEmpty) Heap.no_max_value()
    else if (left.isEmpty) right.max
    else if (right.isEmpty) left.max
    else order.max(left.max, right.max)

  override val depth: Int = math.max(left.depth, right.depth) + 1

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

  def insert[T](value: T, heap: Heap[T])
               (implicit
                order: Ordering[T]): Heap[T] = heap match {
    case BinaryHeap(left, right) =>
      if (left.depth < right.depth)
        cons(insert(value, left), right)
      else
        cons(left, insert(value, right))
    case x@LeafHeap(_)           =>
      if ((x.value.hashCode() ^ value.hashCode()) % 2 == 0)
        BinaryHeap(x, LeafHeap(value))
      else
        BinaryHeap(LeafHeap(value), x)

    case EmptyHeap => LeafHeap(value)
  }


  def merge[T](main: Heap[T], from: Heap[T])
              (implicit
               order: Ordering[T]): Heap[T] ={
    main match {
      case EmptyHeap => from
      case LeafHeap(x) => insert(x,from)
      case BinaryHeap(left,right) =>
        if (left.depth > right.depth && left.depth > from.depth)
          BinaryHeap(left,BinaryHeap(right,from))
        else if (right.depth > left.depth && right.depth > from.depth)
          BinaryHeap(right,BinaryHeap(left,from))
        else if (from.depth > left.depth && from.depth > right.depth)
          BinaryHeap(from, BinaryHeap(left,right))
        else
          BinaryHeap(left,BinaryHeap(right,from))
    }
  }
//    from match {
//      case EmptyHeap               => main
//      case LeafHeap(x)             => insert(x, main)
//      case BinaryHeap(left, right) => merge(merge(main, left), right)
//    }

  //    if (from.isEmpty)
  //      main
  //    else
  //      merge(insert(from.max, main), dropMax(from))

  //  from.values.foldLeft(main)((heap,value)=> insert(value,heap))


}



