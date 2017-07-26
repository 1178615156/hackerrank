package functionalProgramming.persistent

import struct.Heap
import struct.Heap.Heap

/**
  * Created by yujieshui on 2017/7/22.
  */
object BoleynSalary {

  import scala.collection.immutable.TreeSet

  case class Relation(client: Int, parent: Int)

  def tree2string[T](tree: Tree[T]): String = tree match {
    case Node(i, client) =>
      val clientString = client.map(_.toString.split("\n").map(s => "  " + s).mkString("\n")).mkString("\n")
      s"""+$i:
         |${clientString}""".stripMargin
    case Leaf(i)         => s"-$i"
  }

  trait Tree[T] {
    implicit val ordered: Ordering[T]

    def depth: Int

    def value: T

    def clientAt(i: Int): T

  }

  case class Node[T](value: T, client: Seq[Tree[T]])(implicit val ordered: Ordering[T]) extends Tree[T] {
    val depth: Int = client.map(_.depth).max + 1
    lazy val clientSeq: Heap[T] =
      Heap.merges(
        client.map {
          case x@Leaf(_)    => x.clientHeap
          case x@Node(_, _) => Heap.insert(x.value, x.clientSeq)
        }
      )


    override def clientAt(i: Salary): T = ???
  }

  case class Leaf[T](value: T)(implicit val ordered: Ordering[T]) extends Tree[T] {
    val depth               = 1
    val clientHeap: Heap[T] = Heap(Seq(value))

    override def clientAt(i: Salary): T = ???
  }

  type Salary = Int

  case class Employ(id: Int, salary: Salary) {
    override def toString: String = s"$id($salary)"
  }

  implicit val ordering: Ordering[Employ] = Ordering.by[Employ, Int](e => e.salary)

  def relation2map(seq: Seq[Relation]): Map[Int, Seq[Int]] =
    seq.groupBy(_.parent).mapValues(_.map(_.client))

  def mkTree(id: Int, map: Map[Int, Seq[Int]], salary: Map[Int, Salary]): Tree[Employ] = {
    if(!map.contains(id))
      Leaf(Employ(id, salary(id)))
    else
      Node(Employ(id, salary(id)), map(id).map(i => mkTree(i, map, salary)))
  }

  def treeIdIndex(tree: Tree[Employ]): Seq[(Int, Tree[Employ])] = {
    tree match {
      case x@Leaf(employ)         => Seq(employ.id -> x)
      case x@Node(employ, client) =>
        (employ.id -> x) +: client.flatMap(tree => treeIdIndex(tree))
    }
  }

  def solution(seq: Seq[Relation], salary: Map[Int, Salary], query: Seq[(Int, Int)]) = {
    val relationMap = relation2map(seq)
    val tree = mkTree(1, relationMap, salary)
    val treeIdIndex: Map[Salary, Tree[Employ]] =
      this.treeIdIndex(tree).toMap
    query.foldLeft(Seq(0)) {
      case (acc, (id, by)) =>
        val employ = treeIdIndex(id + acc.head).clientAt(by - 1)
        employ.id +: acc
    }.reverse.tail
  }

  def readListInt(): List[Int] = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: q :: Nil = readListInt()
    val relation = 1 until n map (_ => readListInt() match {
      case client :: parent :: Nil => Relation(client, parent)
    })
    val salary = (1 to n) zip readListInt()
    val query = 1 to q map (_ => readListInt() match {
      case a :: b :: Nil => a -> b
    })
    val result =
      solution(relation, salary.toMap, query)

    println(result.mkString("\n"))
  }

}
