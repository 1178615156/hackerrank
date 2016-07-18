package algorithms.grahp

import java.io.File

import utils.SetInt

import scala.io.Source

/**
  * Created by yuJieShui on 2016/7/17.
  */
object MST {

  import GraphEntity._

  case class Result(node: Node[Int], minWight: Weight, totalWight: Weight,edge: Edge[Int])

  def mst(graph: Graph[Int],
          startNode: Node[Int], startWeight: Weight,
          rt: Map[Node[Int], Result],
          closeList: Seq[Node[Int]] = Nil,
          closeEdgeList:Seq[Edge[Int]] = Nil

         ): Map[Node[Int], Result] = {
    val nextEdge = graph.aroundEdge(startNode).groupBy(_.end).map {
      case (key, value) => value.sortBy(_.weight).head
    }.filterNot(e=>closeEdgeList.contains(e) || closeEdgeList.contains(e.copy(begin = e.end,end = e.begin)))

    val nextNodeResult = nextEdge.collect {
      case x@Edge(_, end, weight) if rt.contains(end) && weight < rt(end).minWight =>
        Result(end, weight, startWeight + weight,x)
      case x@Edge(_, end, weight) if !rt.contains(end) =>
        Result(end, weight, startWeight + weight,x )
    }.filterNot(e => closeList.contains(e.node)).toSeq

    if (nextNodeResult.isEmpty)
      rt
    else
      nextNodeResult.foldLeft(rt ++ nextNodeResult.map(e => e.node -> e).toMap) {
        case (acc, Result(node, minWight, totalWight,edge)) =>
          val r =
            mst(graph, node, totalWight, acc, nod e +: closeList,nextNodeResult.map(_.edge) ++ closeEdgeList)
          println(r.toSeq.sortBy(_._1).mkString("\n"))
          println("----------------------------------")
          r
      }
  }

  def solution(graph: Graph[Int], startNode: Node[Int]): Int = {
    mst(graph, startNode, 0, Map(startNode -> Result(startNode, 0, 0,Edge(startNode,startNode))), Seq(startNode))
      .values.map(_.edge.weight).sum
  }

  def main(args: Array[String]) {

    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

    val n :: l :: Nil = readListInt()
    val edges = 1 to l map { _ =>
      val begin :: end :: wight :: Nil = readListInt()
      Edge(begin, end, wight)
    }
    val startNode = readListInt().head
    val nodes = 1 to n
    val graph = Graph(edges, nodes)

    val out = solution(graph, startNode)
    println(out)
  }

  SetInt(
    Source.fromFile(new File("D:\\git\\hackerrank\\src\\main\\resources\\grahp_mst_4")).getLines().mkString("\n")
  )
}
