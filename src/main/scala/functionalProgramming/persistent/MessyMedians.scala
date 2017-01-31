package functionalProgramming.persistent

import struct.BinaryTree

/**
  * Created by yujieshui on 2016/9/9.
  */


class MessyMedians {

  import BinaryTree._

  type Struct = BinaryTree[Int]

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

  def emptyStruct: Struct = Empty[Int]()

  def needPersistent(seq: Seq[Int]): Set[Int] =
    ((1 to seq.size) zip seq filter (_._2 < 0) map { case (i, r) => i + r }).toSet

  def solution(seq: Seq[Int]): Seq[Int] = {
    val needPersistent = this.needPersistent(seq)
    val (result, _, _) =
      ((1 to seq.size) zip seq).foldLeft((Seq[Int](), emptyStruct, Map[Int, Struct]())) {
        case ((result, struct, cache), (index, element)) =>

          val new_struct =
            if(element > 0) addByOrder(element)(struct) else cache(index + element)

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
