package struct

/**
  * Created by yujieshui on 2017/1/16.
  */
object BinaryTree {

  trait BinaryTree[T] {

    implicit def ordering: Ordering[T]

    def isEmpty: Boolean

    def size: Int

    def max: T

    def height: Int
  }

  final case class Leaf[T](value: T)(implicit override val ordering: Ordering[T]) extends BinaryTree[T] {
    override val isEmpty: Boolean = false
    override val size   : Int     = 1
    override val max    : T       = value
    override val height : Int     = 1
  }

  final case class Node[T](left: BinaryTree[T], right: BinaryTree[T])(implicit override val ordering: Ordering[T]) extends BinaryTree[T] {
    override val isEmpty: Boolean = false
    override val size   : Int     = left.size + right.size
    override val max    : T       = ordering.max(left.max, right.max)
    override val height : Int     = math.max(left.height, right.height) + 1
  }

  final case class Empty[T]()(implicit override val ordering: Ordering[T]) extends BinaryTree[T] {
    override val isEmpty: Boolean = true
    override val size   : Int     = 0
    override val height : Int     = 0

    override def max: T = throw new NoSuchElementException("empty have not max")
  }


  def needReshape[T](binaryTree: BinaryTree[T]): Boolean = {
    binaryTree match {
      case Node(left, right) if math.abs(left.height - right.height) > 1 => true
      case _                                                             => false
    }
  }

  def reshape[T: Ordering](binaryTree: BinaryTree[T]): BinaryTree[T] = {
    if(needReshape(binaryTree)) {
      binaryTree match {
        case Node(left@Node(ll, lr), right) if left.height > right.height && ll.height >= lr.height               =>
          Node(ll, Node(lr, right))
        case Node(left@Node(ll, lr@Node(lrl, lrr)), right) if left.height > right.height && ll.height < lr.height =>
          Node(Node(ll, lrl), Node(lrr, right))

        case Node(left, right@Node(rl, rr)) if left.height < right.height && rr.height >= rl.height               =>
          Node(Node(left, rl), rr)
        case Node(left, right@Node(rl@Node(rll, rlr), rr)) if left.height < right.height && rr.height < rl.height =>
          Node(Node(left, rll), Node(rlr, rr))
      }
    }
    else binaryTree
  }

  def addByOrder[T: Ordering](i: T)(binaryTree: BinaryTree[T]): BinaryTree[T] = binaryTree match {
    case Empty()                                                      => Leaf(i)
    case Leaf(value) if implicitly[Ordering[T]].lt(i, value)          => Node(Leaf(i), Leaf(value))
    case Leaf(value) if implicitly[Ordering[T]].gteq(i, value)        => Node(Leaf(value), Leaf(i))
    case Node(left, right) if implicitly[Ordering[T]].lt(i, left.max) => reshape(Node(addByOrder(i)(left), right))
    case Node(left, right)                                            => reshape(Node(left, addByOrder(i)(right)))
  }

  def addByHeight[T](i: T)(binaryTree: BinaryTree[T]): BinaryTree[T] = {
    import binaryTree.ordering
    binaryTree match {
      case Empty()                                          => Leaf(i)
      case Leaf(value)                                      => Node(Leaf(value), Leaf(i))
      case Node(left, right) if left.height <= right.height => reshape(Node(addByHeight(i)(left), right))
      case Node(left, right)                                => reshape(Node(left, addByHeight(i)(right)))
    }
  }
}
