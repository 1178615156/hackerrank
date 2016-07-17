package algorithms.grahp

/**
  * Created by yujieshui on 2016/7/15.
  */
object GraphEntity {
  type Distances = Int

  type Node[T] = T

  type Layer[T] = Seq[Node[T]]

  case class Edge[T](begin: Node[T], end: Node[T])

  case class NodeDistances[T](node: Node[T], distances: Distances)

  case class Graph[T](edges: Seq[Edge[T]], nodes: Seq[Node[T]]) {

    val cacheEdge = edges ++ edges.map(e => e.copy(begin = e.end, end = e.begin)) groupBy (_.begin) mapValues (_.distinct)
    val cacheNode = cacheEdge.mapValues(_.map(_.end).toSet)

    def aroundEdge(node: Node[T]): Seq[Edge[T]] = cacheEdge.get(node).getOrElse(Nil)

    def aroundNode(node: Node[T]): Set[Node[T]] = cacheNode.get(node).getOrElse(Set())
  }

}
