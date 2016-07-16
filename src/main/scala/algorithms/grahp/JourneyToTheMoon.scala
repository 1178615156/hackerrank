package algorithms.grahp

import utils.SetInt

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

object JourneyToTheMoon {

  import GraphEntity._

  ///////////////////////////////////////////////////////////////////////////////////////

  type Astronaut = Int

  type Country[T] = Set[T]

  def sameCountry(astronaut: Astronaut, graph: Graph[Astronaut], rt: Country[Astronaut] = Set()): Country[Astronaut] = {
    val aroundAstronaut: Set[Node[Astronaut]] = graph.aroundNode(astronaut)

    val thisCountry = aroundAstronaut diff rt

    if (thisCountry.isEmpty)
      rt + astronaut
    else {
      thisCountry.foldLeft((rt + astronaut) ++ thisCountry) {
        case (acc, otherAstronaut) => sameCountry(otherAstronaut, graph, acc)
      }
    }
  }

  def differentCountryAstronaut(graph: Graph[Astronaut], astronauts: Country[Astronaut], rt: Seq[Country[Astronaut]] = Seq()): Seq[Country[Astronaut]] = {
    if (astronauts.isEmpty) {
      rt
    } else {
      val astronaut = astronauts.head
      val astronautCountry: Country[Astronaut] = sameCountry(astronaut, graph)
      val otherAstronaut = astronauts diff astronautCountry
      differentCountryAstronaut(graph, otherAstronaut, astronautCountry +: rt)
    }
  }

  def combination(l: Seq[Country[Astronaut]]): Long = {
    def c2(total: Int): Long = if (total < 2) 0 else total.toLong * (total.toLong - 1) / 2

    def impl2Optimize(l: Seq[Int], sum: Int): Long = c2(sum) - l.map(c2).sum

    impl2Optimize(l.map(_.size), l.map(_.size).sum)
  }


  def solution(graph: Graph[Astronaut], astronauts: Seq[Astronaut]): Long = {
    val allCountryAstronaut = differentCountryAstronaut(graph, astronauts.toSet)
    combination(allCountryAstronaut)
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: l :: Nil = readListInt()
    val edges = 1 to l map { _ =>
      val begin :: end :: Nil = readListInt()
      Edge(begin, end)
    }
    val astronauts = 0 until n
    val graph = Graph(edges, astronauts)
    val out =
      solution(graph, astronauts)
    println(out)
  }


  SetInt(
    """4 2
      |0 1
      |2 3
    """.stripMargin)
}















