package struct

/**
  * Created by yujieshui on 2017/1/16.
  */

trait BinaryTreeOpts {

  trait BinaryTree[T] {

    implicit def ordering: Ordering[T]

    val isEmpty: Boolean = this match {
      case Empty() => true
      case _       => false
    }

    val size: Int = this match {
      case Empty()           => 0
      case Leaf(_)           => 1
      case Node(left, right) => left.size + right.size
    }

    val height: Int = this match {
      case Empty()           => 0
      case Leaf(_)           => 1
      case Node(left, right) => math.max(left.height, right.height) + 1
    }

    def max: T

    def min: T

    def notEmpty: Boolean = !isEmpty

    def depth: Int = height
  }

  final case class Leaf[T](value: T)(implicit override val ordering: Ordering[T]) extends BinaryTree[T] {
    override val isEmpty: Boolean = false
    override val size   : Int     = 1
    override val max    : T       = value
    override val min    : T       = value
  }

  final case class Empty[T]()(implicit override val ordering: Ordering[T]) extends BinaryTree[T] {
    override def max: T = notMaxElement()

    override def min: T = notMinElement()
  }

  final case class Node[T](left: BinaryTree[T], right: BinaryTree[T])(implicit override val ordering: Ordering[T]) extends BinaryTree[T] {

    override lazy val max: T =
      if(left.isEmpty && right.isEmpty) notMaxElement()
      else if(left.isEmpty) right.max
      else if(right.isEmpty) left.max
      else ordering.max(left.max, right.max)

    override lazy val min: T =
      if(left.isEmpty && right.isEmpty) notMinElement()
      else if(left.isEmpty) right.min
      else if(right.isEmpty) left.min
      else ordering.min(left.min, right.min)
  }


  final def notMaxElement() = throw new NoSuchElementException("empty have not max")

  final def notMinElement() = throw new NoSuchElementException("empty have not min")

  def node[T](left: BinaryTree[T], right: BinaryTree[T]): BinaryTree[T] = {
    import left.ordering
    if(left.notEmpty && right.notEmpty)
      Node(left, right)
    else
    if(left.isEmpty) right else left
  }

  def empty[T: Ordering]: Empty[T] = Empty[T]()

  def needReshape[T](binaryTree: BinaryTree[T]): Boolean = binaryTree match {
    case Node(left, right) if math.abs(left.height - right.height) > 1 => true
    case _                                                             => false
  }


  def reshape[T](binaryTree: BinaryTree[T]): BinaryTree[T] = {
    import binaryTree.ordering
    if(needReshape(binaryTree)) reshape(binaryTree match {
      case Node(left@Node(ll, lr), right) if left.height > right.height && ll.height >= lr.height               =>
        Node(ll, node(lr, right))
      case Node(left@Node(ll, lr@Node(lrl, lrr)), right) if left.height > right.height && ll.height < lr.height =>
        Node(node(ll, lrl), node(lrr, right))

      case Node(left, right@Node(rl, rr)) if left.height < right.height && rr.height >= rl.height               =>
        Node(node(left, rl), rr)
      case Node(left, right@Node(rl@Node(rll, rlr), rr)) if left.height < right.height && rr.height < rl.height =>
        Node(node(left, rll), node(rlr, rr))
    })
    else binaryTree
  }

  def addByHeight[T](i: T)(binaryTree: BinaryTree[T]): BinaryTree[T] = {
    import binaryTree.ordering
    binaryTree match {
      case Empty()           => Leaf(i)
      case Leaf(value)       => Node(Leaf(value), Leaf(i))
      case Node(left, right) =>
        if(left.height > right.height)
          node(left, addByHeight(i)(right))
        else
          node(addByHeight(i)(left), right)
    }
  }

  def addByOrder[T](i: T)(binaryTree: BinaryTree[T]): BinaryTree[T] = {
    import binaryTree.ordering
    binaryTree match {
      case Empty()                                                      => Leaf(i)
      case Leaf(value) if implicitly[Ordering[T]].lt(i, value)          => node(Leaf(i), Leaf(value))
      case Leaf(value) if implicitly[Ordering[T]].gteq(i, value)        => node(Leaf(value), Leaf(i))
      case Node(left, right) if implicitly[Ordering[T]].lt(i, left.max) => reshape(node(addByOrder(i)(left), right))
      case Node(left, right)                                            => reshape(node(left, addByOrder(i)(right)))
    }
  }

  def get[T](i: Int, tree: BinaryTree[T]): T = {
    tree match {
      case Leaf(v) if i == 0 => v
      case Node(left, right) =>
        if(left.size >= i + 1)
          get(i, left)
        else {
          get(i - left.size, right)
        }
      case Empty()           => ???
      case _                 => ???
    }
  }

  def cons[T](left: BinaryTree[T], right: BinaryTree[T]): BinaryTree[T] = node(left, right)

}

object Heap extends BinaryTreeOpts {
  type Heap[T] = BinaryTree[T]


  def merge[T](main: Heap[T], from: Heap[T]): Heap[T] = from match {
    case Empty()           => main
    case Leaf(x)           => insert(x, main)
    case Node(left, right) => merge(merge(main, left), right)
  }

  def merges[T: Ordering](seq: Seq[Heap[T]]): Heap[T] = {
    if(seq.isEmpty) empty[T]
    else if(seq.tail.isEmpty) seq.head
    else
      merges(
        seq.grouped(2).toSeq.map {
          case Seq(a, b) => cons(a, b)
          case Seq(a)    => a
        }
      )
  }

  def dropMin[T: Ordering](heap: Heap[T]): Heap[T] = {
    def impl(heap: Heap[T]): Heap[T] =
      heap match {
        case Node(left, right) if left.isEmpty  => impl(right)
        case Node(left, right) if right.isEmpty => impl(left)
        case Node(left, right)                  =>
          implicitly[Ordering[T]].compare(left.min, right.min) match {
            case e if e > 0  => cons(left, impl(right))
            case e if e == 0 => cons(impl(left), right)
            case e if e < 0  => cons(impl(left), right)
          }
        case Empty()                            => notMinElement()
        case Leaf(x)                            => Empty[T]()
      }

    reshape(impl(heap))
  }

  //scala.collection.immutable.Heap

  def dropMax[T](heap: Heap[T]): Heap[T] = {
    import heap.ordering
    def impl(heap: Heap[T]): Heap[T] =
      heap match {
        case Node(left, right) if left.notEmpty && right.notEmpty =>
          heap.ordering.compare(left.max, right.max) match {
            case e if e < 0 => reshape(cons(left, impl(right)))
            case _          => reshape(cons(impl(left), right))
          }
        case Node(Empty(), right)                                 => impl(right)
        case Node(left, Empty())                                  => impl(left)

        case Leaf(x) => Empty[T]()
        case Empty() => notMaxElement()
      }

    reshape(impl(heap))
  }

  def apply[T: Ordering](seq: Seq[T]): Heap[T] = seq match {
    case Seq()         => Empty[T]()
    case head +: Seq() => Leaf(head)
    case other         =>
      val (l, r) = other.splitAt(other.size / 2)
      cons(apply(l), apply(r))
  }

  def single[T: Ordering](t: T): Heap[T] = Leaf(t)


  def insert[T](value: T, heap: Heap[T]): BinaryTree[T] = addByHeight(value)(heap)

}

object BinaryTree extends BinaryTreeOpts {

  def insert[T](value: T, binaryTree: BinaryTree[T]): BinaryTree[T] = addByOrder(value)(binaryTree)
}
