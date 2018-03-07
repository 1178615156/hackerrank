package algorithms.search

/**
  * Created by yujieshui on 2017/6/27.
  */
object SimilarPair {

  import scala.collection.immutable.TreeSet
  import scala.collection.Searching._

  case class Node(parent: Int, value: Int)

  type Value = Int
  type Nodes = Map[Int, Seq[Value]]

  def root(n: Int, seq: Seq[Node]): Set[Value] = {
    (1 to n).toSet -- seq.map(_.value)
  }

  def clients(value: Value, nodes: Nodes): Seq[Value] = {
    nodes.getOrElse(value, Nil)
  }

  def similarPairNum(k: Int, seq: TreeSet[Int]) = {
    val list = seq.toList
    seq.toList.map { i =>
      val start = list.search(i - k)
      val end = list.search(i + k)

      end match {
        case Found(x)          => x - start.insertionPoint + 1
        case InsertionPoint(x) => x - start.insertionPoint
      }
    }.sum

  }

  def impl3(node: Int,
            parentValue: TreeSet[Int],
            nodes: Nodes,
            k: Int): Int = {
    val clients = this.clients(node, nodes)
    val newParentValue = parentValue + node
    if(clients.isEmpty) {
      similarPairNum(k, newParentValue)
    } else {
      clients.map(e => impl3(e, newParentValue, nodes, k)).sum
    }
  }

  def impl2(node: Int,
            parentValue: TreeSet[Int],
            nodes: Nodes,
            k: Int): Int = {
    val clients = this.clients(node, nodes)
    if(clients.isEmpty)
      0
    else {
      val newParentValue = parentValue + node
      val similarPairNum = clients.map(c => newParentValue.range(c - k, c + k + 1).size)
      similarPairNum.sum + clients.map(c => impl2(c, newParentValue, nodes, k)).sum
    }
  }

  def impl(node: Int,
           parentValue: TreeSet[Int],
           nodes: Nodes,
           k: Int): Int = {
    val similarPairNum = parentValue.range(node - k, node + k + 1).size
    val clients = this.clients(node, nodes)
    val newParentValue = parentValue + node
    if(clients.isEmpty)
      similarPairNum
    else
      clients.map(e => impl(e, newParentValue, nodes, k)).sum + similarPairNum
  }

  def solution(n: Int, k: Int, seq: Seq[Node]): Int = {
    val nodes = seq.groupBy(_.parent).mapValues(_.map(_.value))
    val root = this.root(n, seq)
    val result = impl3(root.head, TreeSet(), nodes, k)
    result
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: k :: Nil = readListInt()
    val nodes = 1 until n map (e => readListInt() match {
      case a :: b :: Nil => Node(a, b)
    })
    val result = solution(n, k, nodes)
    println(result)
  }
}
