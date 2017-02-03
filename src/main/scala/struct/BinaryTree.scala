package struct

/**
  * Created by yujieshui on 2017/1/16.
  */


trait BinaryTreeOpts {

  trait BinaryTree[T] {

    implicit def ordering: Ordering[T]

    def isEmpty: Boolean
    def notEmpty = !isEmpty

    def size: Int

    def height: Int

    def max: T

    def depth = height
  }

  final case class Leaf[T](value: T)(implicit override val ordering: Ordering[T]) extends BinaryTree[T] {
    override val isEmpty: Boolean = false
    override val size   : Int     = 1
    override val max    : T       = value
    override val height : Int     = 1
  }

  final case class Node[T](left: BinaryTree[T], right: BinaryTree[T])(implicit override val ordering: Ordering[T]) extends BinaryTree[T] {
    override      val isEmpty: Boolean = false
    override lazy val size   : Int     = left.size + right.size
    override lazy val height : Int     = math.max(left.height, right.height) + 1
    override lazy val max    : T       =
      if(left.isEmpty && right.isEmpty) notMaxElement()
      else if(left.isEmpty) right.max
      else if(right.isEmpty) left.max
      else ordering.max(left.max, right.max)
  }

  final case class Empty[T]()(implicit override val ordering: Ordering[T]) extends BinaryTree[T] {
    override val isEmpty: Boolean = true
    override val size   : Int     = 0
    override val height : Int     = 0

    override def max: T = notMaxElement()
  }

  def node[T](left: BinaryTree[T], right: BinaryTree[T])(implicit ordering: Ordering[T]): BinaryTree[T] = {
    if(left.notEmpty && right.notEmpty)
      Node(left,right)
    else
    if(left.isEmpty) right else left
  }

  def notMaxElement() = throw new NoSuchElementException("empty have not max")

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
