package functionalProgramming.persistent


/**
  * Created by yujieshui on 2016/9/9.
  */
class MessyMedians {
  type Struct = BinaryTree[Int]

  trait BinaryTree[T] {

    def ordering: Ordering[T]

    def isEmpty: Boolean = this match {
      case Empty() => true
      case _       => false
    }

    lazy val size: Int = this match {
      case Empty()           => 0
      case Leaf(_)           => 1
      case Node(left, right) => left.size + right.size
    }

    lazy val max: T = this match {
      case Empty()           => throw new NoSuchElementException("empty have not max")
      case Leaf(x)           => x
      case Node(left, right) => ordering.max(left.max, right.max)
    }
  }

  case class Leaf[T](value: T)(implicit override val ordering: Ordering[T]) extends BinaryTree[T]

  case class Node[T](left: BinaryTree[T], right: BinaryTree[T])(implicit override val ordering: Ordering[T]) extends BinaryTree[T]

  case class Empty[T]() extends BinaryTree[T] {
    override def ordering = ???
  }

  def emptyStruct: Struct = Empty[Int]()

  def add(i: Int)(struct: Struct): Struct = struct match {
    case Empty()                             => Node(Leaf(i), Empty())
    case Leaf(value) if i < value            => Node(Leaf(i), Leaf(value))
    case Leaf(value) if i >= value           => Node(Leaf(value), Leaf(i))
    case Node(Empty(), Empty())              => Node(Leaf(i), Empty())
    case Node(Empty(), right)                =>
      if(i < right.max)
        Node(Leaf(i), right)
      else
        Node(Empty(), add(i)(right))
    case Node(left, Empty())                 =>
      if(i < left.max)
        Node(add(i)(left), Empty())
      else
        Node(left, Leaf(i))
    case Node(left, right) if i < left.max  => Node(add(i)(left), right)
    case Node(left, right)=> Node(left, add(i)(right))

  }

  def medina(struct: Struct): Int = {
    def impl(struct: Struct, goal: Int): Int = {
      struct match {
        case Leaf(value)                            => value
        case Node(left, right) if left.size >= goal => impl(left, goal)
        case Node(left, right) if left.size < goal  => impl(right, goal - left.size)
      }
    }

    impl(struct, (struct.size + 1) / 2)
  }

  //    struct((struct.size - 1) / 2)


  def needPersistent(seq: Seq[Int]): Set[Int] =
    ((1 to seq.size) zip seq filter (_._2 < 0) map { case (i, r) => i + r }).toSet

  def solution(seq: Seq[Int]): Seq[Int] = {
    val needPersistent = this.needPersistent(seq)
    val (result, _, _) =
      ((1 to seq.size) zip seq).foldLeft((Seq[Int](), emptyStruct, Map[Int, Struct]())) {
        case ((result, struct, cache), (index, element)) =>

          val new_struct =
            if(element > 0) add(element)(struct) else cache(index + element)

          def new_cache = cache + (index -> new_struct)

          val new_result = medina(new_struct) +: result
          (new_result, new_struct, if(needPersistent contains index) new_cache else cache)
      }
    result.reverse
  }
}

object MessyMedians extends MessyMedians {
  def readListInt(): List[Int] = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val data = 1 to n map (_ => readListInt().head)
    val result = solution(data)
    println(
      result.mkString("\n")
    )
  }


}
