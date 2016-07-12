package algorithms.grahp

/**
  * Created by yuJieShui on 2016/5/16.
  */

object Entity {
  type Distances = Int

  case class Node[T](value: T)

  case class Edge[T](begin: Node[T], end: Node[T], distances: Distances) {
    def containNode(node: Node[T]) = begin == node || end == node

    def sweepToBeginOf(newBegin: Node[T]) = if (newBegin == begin) this else Edge(end, begin, distances)
  }

  case class Layer[T](value: Seq[Node[T]])

  case class NodeDistances[T](node: Node[T], distances: Distances)

  case class Grahp[T](edges: Seq[Edge[T]], nodes: Seq[Node[T]]) {
    val cacheEdge = edges ++ edges.map(e=>e.copy(begin = e.end,end = e.begin)) groupBy(_.begin) mapValues(_.sortBy(_.distances).distinct)
    def aroundEdge(node: Node[T]): Seq[Edge[T]] = {
      cacheEdge.get(node).getOrElse(Nil)
    }
  }

}
