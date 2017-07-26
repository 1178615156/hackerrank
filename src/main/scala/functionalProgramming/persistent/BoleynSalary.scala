package functionalProgramming.persistent

import struct.Heap
import utils.SetInt

/**
  * Created by yujieshui on 2017/7/22.
  */
object BoleynSalary {

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

    def clientAt(i: Int): T
  }

  case class Node[T](value: T, client: Seq[Tree[T]])(implicit val ordered: Ordering[T]) extends Tree[T] {

    def margeSoredSeq2(l: Seq[T], r: Seq[T], rt: Seq[T]): Seq[T] = {
      if(l.isEmpty) rt.reverse ++ r
      else if(r.isEmpty) rt.reverse ++ l
      else if(ordered.gt(l.head, r.head))
        margeSoredSeq2(l, r.tail, r.head +: rt)
      else
        margeSoredSeq2(l.tail, r, l.head +: rt)
    }

    def margeSoredSeq(seq: Seq[Seq[T]]): Seq[T] = {
      if(seq.isEmpty) Seq()
      else if(seq.tail.isEmpty) seq.head
      else {
        margeSoredSeq(seq.grouped(2).toSeq.map(e =>
          if(e.tail.isEmpty) e.head
          else
            margeSoredSeq2(e(0), e(1), Seq()))
        )
      }
    }

    val clientHeap: Vector[T] = {
      val value: Seq[T] = client.collect {
        case x@Leaf(_)    => x.value
        case x@Node(_, _) => x.value
      }
      val clientValue = client.collect { case x@Node(_, _) => x.clientHeap }
      margeSoredSeq((value.sorted +: clientValue).filter(_.nonEmpty)).toVector
    }

    override def clientAt(i: Salary): T = {
      clientHeap.apply(i)
    }
  }

  case class Leaf[T](value: T)(implicit val ordered: Ordering[T]) extends Tree[T] {
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


  SetInt(
    """8 7
      |2 1
      |3 2
      |4 2
      |7 4
      |8 4
      |5 1
      |6 5
      |70 40 60 80 10 20 30 50
      |2 1
      |-6 5
      |-4 1
      |-5 3
      |2 1
      |-5 4
      |2 2
      |
    """.stripMargin)
}
