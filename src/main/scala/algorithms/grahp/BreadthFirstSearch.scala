package algorithms.grahp

import java.io.File

import utils.SetInt

import scala.collection.immutable.IndexedSeq
import scala.concurrent.Future
import scala.io.Source

//  implicit class WithSequenceInFutureOption[T](val future: Option[Future[T]]){
//    def sequence = Future.sequence(future.toList)
//  }
//  implicit class WithSequenceInFutureList[T](val future: List[Future[T]]){
//    def sequence = Future.sequence(future.toList)
//  }
//  Some(Future(1)).sequence
//  List(Future(1)).sequence


object BreadthFirstSearch {

  type Node = Int
  type Distances = Int
  type Layer = Seq[Node]

  case class NodeDistances(distances: Distances, node: Node)

  case class Edge(begin: Node, end: Node) {
    def containNode(node: Node) = begin == node || end == node
  }

  def bfs(openList: Seq[Edge], closeList: Seq[Layer]): Seq[Layer] = {
    lazy val clientLayer: Layer = closeList.last.flatMap(current => {
      val clientEdge = openList.filter(_.containNode(current))
      val clientNode = clientEdge.map(e => if (e.end == current) Edge(e.end, e.begin) else e).map(_.end)
      clientNode
    }).distinct.filterNot(node => closeList.exists(_.contains(node)))

    if (openList.isEmpty || clientLayer.isEmpty)
      closeList
    else
      bfs(openList.filterNot(edge => closeList.flatten.exists(edge.containNode)), closeList :+ clientLayer)
  }

  def solution(start: Node, edge: Seq[Edge], allNode: Seq[Node]): Seq[Distances] = {
    val tree = bfs(edge, Seq(Seq(start)))
    val clientTree = tree.tail
    val unreachable =
      allNode.filterNot(node => tree.flatten.contains(node))
        .map(node => NodeDistances(-1, node))

    val reachable =
      1 to clientTree.size map (_ * 6) zip clientTree map {
        case (distances, layer) =>
          layer.map(node => NodeDistances(distances = distances, node = node))
      }

    reachable.flatten ++ unreachable sortBy (_.node) map (_.distances)
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val size :: Nil = readListInt()
    val data = 1 to size map { _ =>
      val nodeSize :: edgeSize :: Nil = readListInt()
      val edgeList = 1 to edgeSize map { _ =>
        val begin :: end :: Nil = readListInt()
        Edge(begin, end)
      }
      val startNode :: Nil = readListInt()

      (startNode, nodeSize, edgeList)
    }

    val result: IndexedSeq[Seq[Distances]] =
      data map {
        case (startNode, nodeSize, edgeList) => solution(startNode, edgeList, 1 to nodeSize)
      }
    println(
      result.map(_.mkString(" ")).mkString("\n")
    )
  }

  //test case 4
  //test case 2
  SetInt(
    Source.fromFile(new File("D:\\git\\hackerrank\\src\\main\\resources\\grahp_BreadthFirstSearch")).getLines().mkString("\n")
  )
  //  6 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12
  //  6 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 12 12 12 12 12 12 12 12 12 12 12 12 12 12
  def a[T:Numeric](n: T) = {implicitly[Numeric[T]].toInt(n)}
  class Hello
  implicit val x : Numeric[Hello] = ???
  a(new Hello)
}

















