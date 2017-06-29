package algorithms.search



/**
  * Created by yujieshui on 2017/6/27.
  */
object SimilarPair {

  import scala.collection.SortedSet
  import scala.collection.immutable.IntMap

  case class Node(parent: Int, value: Int)

  def root(n: Int, seq: Seq[Node]): Seq[Int] =
    (1 to n) diff seq.map(_.value)

  def leaf(n: Int, seq: Seq[Node]): Seq[Int] =
    (1 to n) diff seq.map(_.parent)

  def toParentMap(seq: Seq[Node]): Map[Int, Int] =
    seq.map(e => e.value -> e.parent).toMap

  def toClientMap(seq: Seq[Node]): Map[Int, Seq[Int]] =
    seq.groupBy(_.parent).mapValues(_.map(_.value))

  def toParentLinkMap(work: Seq[Int],
                      parentMap: Map[Int, Int],
                      clientMap: Map[Int, Seq[Int]],
                      result: Map[Int, SortedSet[Int]]): Map[Int, SortedSet[Int]] = {
    if(work.isEmpty)
      result
    else {
      val client_layer = work.flatMap(e => clientMap.getOrElse(e, Seq()))
      val newResult = result ++ IntMap(client_layer.map(client => (client, result(parentMap(client)) + client)):_* )
      toParentLinkMap(client_layer, parentMap, clientMap, newResult)
    }
  }

  def solution(n: Int, k: Int, seq: Seq[Node]): Int = {
    val parentMap = toParentMap(seq)
    val parentLinkMap =
      toParentLinkMap(root(n, seq), parentMap, toClientMap(seq), IntMap(root(n, seq).map(e => e -> SortedSet(e)): _*))
    (1 to n).foldLeft(0) { case (acc, e) => acc + parentLinkMap(e).range(e - k, e + k + 1).size - 1 }
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
