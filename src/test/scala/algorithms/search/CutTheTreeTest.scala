package algorithms.search

import org.scalatest.{FunSuite, WordSpec}
import CutTheTree._

import scala.collection.immutable

/**
  * Created by yujieshui on 2017/5/24.
  */
class CutTheTreeTest extends WordSpec {
  val values = ("100 200 100 500 100 600".split(" ").map(_.toInt).toVector)
  val edges  = Seq(
    1 -> 2,
    2 -> 3,
    2 -> 5,
    4 -> 5,
    5 -> 6
  )
  val tree   = mkTree(1, (edges ++ edges.map(_.swap)).groupBy(_._1).mapValues(_.map(_._2).toSet).toSeq.sortBy(_._1).map(_._2).toVector, (values), Seq())

  def removeEdge(tree: Tree[Int], start: Int, end: Int): Tree[Int] = tree match {
    case node@Node(i, value, clients) =>
      if(i == start || i == end)
        node.copy(clients = clients.filterNot(e => e.i == start || e.i == end))
      else
        node.copy(clients = clients.map(removeEdge(_, start, end)))

    case _ => tree
  }

  "mu tree" in {
    val n = 50000

    def random: Int = math.abs(math.random * n).toInt

    val edges: Seq[(Int, Int)] = 2 to n map (n => n -> (random % n + 1))
    val values = (1 to n).toVector
    val newEdges: Vector[Set[Int]] = (edges ++ edges.map(_.swap)).groupBy(_._1).mapValues(_.map(_._2).toSet).toSeq.sortBy(_._1).map(_._2).toVector
    val tree = mkTree(n, newEdges, values,List())
    Thread.sleep(1234567)
  }
  "mu solution" in {
    val n = 50000

    def random: Int = math.abs(math.random * n).toInt

    val edges: Seq[(Int, Int)] = 2 to n map (n => n -> (random % n + 1)) //++ (1 to n zip (2 to n))
    solution(n, 1 to n, edges)
  }

  "mk tree" in {
    println(tree)
  }

  "remove edge" must {
    "1,2" in {
      assert(removeEdge(tree, 1, 2).sum === 100)
    }
    "2,3" in {
      assert(removeEdge(tree, 2, 3).sum === 1500)
    }
    "4,5" in {
      assert(removeEdge(tree, 4, 5).sum === 1100)
    }
    "5,6" in {
      assert(removeEdge(tree, 5, 6).sum === 1000)
    }

  }
}
