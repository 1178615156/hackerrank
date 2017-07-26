package struct

/**
  * Created by yujieshui on 2017/1/16.
  */

trait BinaryTreeModelOpts {

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


  def notMaxElement() = throw new NoSuchElementException("empty have not max")

  def notMinElement() = throw new NoSuchElementException("empty have not min")

}

trait BinaryTreeOpts extends BinaryTreeModelOpts {

  def node[T](left: BinaryTree[T], right: BinaryTree[T])(implicit ordering: Ordering[T]): BinaryTree[T] = {
    if(left.notEmpty && right.notEmpty)
      Node(left, right)
    else if(left.isEmpty) right else left
  }

  def empty[T: Ordering]: Empty[T] = Empty[T]()

  def needReshape[T](binaryTree: BinaryTree[T]): Boolean = binaryTree match {
    case Node(left, right) if math.abs(left.height - right.height) > 1 => true
    case _                                                             => false
  }


  def reshape[T: Ordering](binaryTree: BinaryTree[T]): BinaryTree[T] = {
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

  def insert[T](value: T, binaryTree: BinaryTree[T]): BinaryTree[T]
}

object Heap extends BinaryTreeOpts {
  type Heap[T] = BinaryTree[T]

  def cons[T: Ordering](left: Heap[T], right: Heap[T]): Heap[T] = node(left, right)

  def merge[T: Ordering](main: Heap[T], from: Heap[T]): Heap[T] = reshape(cons(main, from))

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
    def impl[T: Ordering](heap: Heap[T]): Heap[T] =
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

  def dropMax[T: Ordering](heap: Heap[T]): Heap[T] = {
    def impl[T: Ordering](heap: Heap[T]): Heap[T] =
      heap match {
        case Node(left, right) if left.isEmpty  => impl(right)
        case Node(left, right) if right.isEmpty => impl(left)
        case Node(left, right)                  =>
          implicitly[Ordering[T]].compare(left.max, right.max) match {
            case e if e < 0  => cons(left, impl(right))
            case e if e == 0 => cons(impl(left), right)
            case e if e > 0  => cons(impl(left), right)
          }
        case Empty()                            => notMaxElement()
        case Leaf(x)                            => Empty[T]()
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


  override def insert[T](value: T, heap: Heap[T]): BinaryTree[T] = reshape(addByOrder(value)(heap))(heap.ordering)

}

object BinaryTree extends BinaryTreeOpts {

  override def insert[T](value: T, binaryTree: BinaryTree[T]): BinaryTree[T] = addByOrder(value)(binaryTree)
}
