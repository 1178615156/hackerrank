package functionalProgramming

/**
  * Created by yujieshui on 2016/8/19.
  */
object BinaryTree {

  //[..)
  trait Tree[+T] {
    def start: Int

    def end: Int

    def isEmpty: Boolean

    def nonEmpty = !isEmpty

    def min: T

    override def toString: String = s"$start $end $min"
  }

  trait Min[T] {
    def min(l: T, r: T): T
  }

  implicit final val intMin: Min[Int] = new Min[Int] {
    override def min(l: Int, r: Int): Int = if (l > r) r else l
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

  case class Binary[T: Min](left: Tree[T],
                                 right: Tree[T],
                                 override val start: Int,
                                 override val end: Int) extends Tree[T] {
    override val min: T =
      if (left.nonEmpty && right.nonEmpty) implicitly[Min[T]].min(left.min, right.min)
      else if (left.isEmpty && right.nonEmpty) right.min
      else if (left.nonEmpty && right.isEmpty) right.min
      else ???

    override def isEmpty: Boolean = false
  }

  def apply[T: Min](seq: Seq[T], start: Int, end: Int): Tree[T] = {
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

  def updateTree[T: Min](index: Int, updateFunc: T => T)(tree: Tree[T]): Tree[T] = {
    tree match {
      case Empty => ???
      case Leaf(v, start, end) if start == index => Leaf(updateFunc(v), start, end)
      case x@Binary(left, right, start, end) =>
        if (index < right.start)
          x.copy(left = updateTree(index, updateFunc)(left))
        else
          x.copy(right = updateTree(index, updateFunc)(right))
    }
  }

  def rightTree[T](start: Int)(tree: Tree[T]): Seq[Tree[T]] = tree match {
    case Binary(left, right, binaryStart, end) =>
      if (start < left.end)
        right +: rightTree(start)(left)
      else
        rightTree(start)(right)

    case x@Leaf(value, leafStart, end) =>
      if (leafStart >= start) Seq(x) else Seq()
    case Empty => Seq()
  }

  def leftTree[T](end: Int)(tree: Tree[T]): Seq[Tree[T]] = tree match {
    case Binary(left, right, start, binaryEnd) =>
      if (end > left.end)
        left +: leftTree(end)(right)
      else
        leftTree(end)(left)
    case x@Leaf(value, start, leafEnd) =>
      if (leafEnd <= end) Seq(x) else Seq()
    case Empty => Seq()
  }

  def subArrTree[T](start: Int, end: Int)(tree: Tree[T]): Seq[Tree[T]] = {
    val r = rightTree(start)(tree)
    val l = leftTree(end)(tree)
    r.filter(_.end < end) ++ l.filter(_.start >= start)
  }

  def tree2value[T](tree: Tree[T]): Seq[T] = tree match {
    case Empty => Seq()
    case Leaf(value, _, _) => Seq(value)
    case Binary(left, right, _, _) => tree2value(left) ++ tree2value(right)
  }
}
