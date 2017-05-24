package algorithms.search

import scala.collection.immutable.HashMap

/**
  * Created by yujieshui on 2017/5/24.
  */
object CutTheTree {

  trait Tree[T] {
    def i: Int

    implicit def numeric: Numeric[T]

    val is: Set[Int] = this match {
      case Node(i, value, clients) => clients.flatMap(_.is).toSet + i
      case Leaf(i, value)          => Set(i)
    }

    val sum: T = this match {
      case Node(_, value, clients) => numeric.plus(value, clients.map(_.sum).sum)
      case Leaf(_, value)          => value
    }

    override def toString: String = this match {
      case Node(i, value, clients) =>
        s"""i=$i ;; v=$value
           |${clients.map(_.toString.split("\n").map("  " + _).mkString("\n")).mkString("\n")}
           |""".stripMargin
      case Leaf(i, value)          =>
        s"i=$i ;; v=$value"
    }

  }

  case class Leaf[T](i: Int, value: T)(implicit val numeric: Numeric[T]) extends Tree[T]

  case class Node[T](i: Int, value: T, clients: Seq[Tree[T]])(implicit val numeric: Numeric[T]) extends Tree[T]


  def mkTree(i: Int,
             edges: Vector[Set[Int]],
             values: Vector[Int],
             parent: Seq[Int]
            ): Tree[Int] = {
    val newParent = i +: parent
    val clients =
      edges(i - 1)
        .collect { case client if !parent.contains(client) => mkTree(client, edges, values, newParent) }
    if(clients.isEmpty)
      Leaf(i, values(i - 1))
    else
      Node(i, values(i - 1), clients.toSeq)
  }

  def clientOfRemoveEdge(tree: Tree[Int], start: Int, end: Int): Option[Tree[Int]] = tree match {
    case node@Node(i, value, clients) if tree.is.contains(start) || tree.is.contains(end) =>
      if(i == start || i == end)
        clients.find(e => e.i == start || e.i == end)
      else
        clients.toStream.map(tree => clientOfRemoveEdge(tree, start, end)).find(_.nonEmpty).flatten
    case _                                                                                => None

  }

  def solution(n: Int, values: Seq[Int], edges: Seq[(Int, Int)]) = {
    val root = edges.flatMap(e => e._1 :: e._2 :: Nil).groupBy(e => e).mapValues(_.size).maxBy(_._2)._1
    val newEdges: Vector[Set[Int]] = (edges ++ edges.map(_.swap)).groupBy(_._1).mapValues(_.map(_._2).toSet).toSeq.sortBy(_._1).map(_._2).toVector
    val tree = mkTree(root, newEdges, values.toVector, List())
    val sum = values.sum
    edges.map { case (start, end) =>
      math.abs(2 * clientOfRemoveEdge(tree, start, end).get.sum - sum)
    }.min

  }


  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val values = readListInt()
    val edges = 1 until n map (_ => readListInt()) map { case a :: b :: Nil => a -> b }
    val result = solution(n, values, edges)
    println(result)
  }
}
