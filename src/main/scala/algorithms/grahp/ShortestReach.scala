package algorithms.grahp

import java.io.File
import utils.SetInt

import scala.collection.immutable.IndexedSeq
import scala.io.Source

/**
  * Created by yuJieShui on 2016/5/18.
  */

object ShortestReach {

  import Entity._


  type IntNode = Node[Int]

  def shortestReachImpl(now_node: Node[Int],
                        now_distances: Int,
                        grahp: Grahp[Int])
                       (history: collection.mutable.Map[IntNode, Distances]): collection.mutable.Map[IntNode, Distances] = {
    val connectNextEdge = grahp.aroundEdge(now_node)

    if (connectNextEdge.isEmpty)
      history
    else {
      val nextNode = connectNextEdge.collect {
        case edge if history.get(edge.end).forall(edge.distances + now_distances < _) =>
          history.update(edge.end, edge.distances + now_distances)
          (edge.end, edge.distances + now_distances)
      }
      nextNode.foldLeft(history) {
        case (current_history, (node, distances)) =>
          shortestReachImpl(node, distances, grahp)(current_history)
      }
    }

  }

  def solution(start: Node[Int], grahp: Grahp[Int]): Seq[NodeDistances[Int]] = {
    val reachable = shortestReachImpl(start, 0, grahp)(collection.mutable.Map(start -> 0)).toList.map {
      case (node, distances) => NodeDistances(node, distances)
    }
    val unreachable = grahp.nodes.filterNot(node => reachable.exists(_.node == node)).map(node => NodeDistances(node, -1))
    val result = (unreachable ++ reachable).filter(_.node.value != start.value).sortBy(_.node.value)
    result
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def readIn(readListInt: () => List[Int]) = {
    val size :: Nil = readListInt()
    val data = 1 to size map { _ =>
      val nodeSize :: edgeSize :: Nil = readListInt()
      val edgeList = 1 to edgeSize map { xxx =>

          val begin :: end :: distances :: Nil = readListInt()
          Edge(Node(begin), Node(end), distances)


      }
      val startNode :: Nil = readListInt()

      (Node(startNode), nodeSize, edgeList)
    }
    data
  }


  def main(args: Array[String]) {

    val startTime = System.currentTimeMillis()
    val result: IndexedSeq[Seq[NodeDistances[Distances]]] =
      readIn(readListInt) map {
        case (startNode, nodeSize, edgeList) =>
          val grahp = Grahp(edgeList, 1 to nodeSize map (e => Node(e)))
          solution(startNode, grahp)
      }
//    println(System.currentTimeMillis() - startTime)

    println(
      result.map(_.map(_.distances).mkString(" ")).mkString("\n")
    )
  }

  SetInt(
    Source.fromFile(new File("D:\\git\\hackerrank\\src\\main\\resources\\grahp_ShortestReach_2.txt")).getLines().mkString("\n")
  )
}
