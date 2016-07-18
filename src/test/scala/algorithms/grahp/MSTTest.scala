package algorithms.grahp

import org.scalatest.FunSuite

import scala.math.abs
import scala.util.Random

/**
  * Created by yuJieShui on 2016/7/17.
  */
class MSTTest extends FunSuite {

  import MST._
  import GraphEntity._

  val emptyEdge = Edge(-1, -1)

  val testData_1 = new {
    val graph = Graph(Seq(
      Edge(1, 2, 3),
      Edge(1, 3, 4),
      Edge(4, 2, 6),
      Edge(5, 2, 2),
      Edge(2, 3, 5),
      Edge(3, 5, 7)
    ), 1 to 5)

    val result    = Map(
      1 -> Result(1, 0, 0,emptyEdge),
      2 -> Result(2, 3, 3,emptyEdge),
      3 -> Result(3, 4, 4,emptyEdge),
      4 -> Result(4, 6, 9,emptyEdge),
      5 -> Result(5, 2, 5,emptyEdge)
    )
    val startNode = 1
  }

  test("mst") {
    import testData_1._

    println(
      mst(graph, 1, 0, Map(1 -> Result(1, 0, 0,emptyEdge))).toSeq.sortBy(_._1).mkString("\n")
    )
    assert(
      mst(graph, 1, 0, Map(1 -> Result(1, 0, 0,emptyEdge))) ===
        result
    )
  }
  test("solution") {
    import testData_1._
    assert(
      solution(graph, startNode) === 15
    )
  }

  test("test 1") {
    val graph = Graph(
      Seq(
        Edge(1, 2, 1),
        Edge(1, 2, 2),
        Edge(1, 2, 3)
      ), 1 to 2
    )
    assert(solution(graph, 1) === 1)
  }
  test("test 2") {
    val graph = Graph(
      Seq(
        Edge(1, 2, 2),
        Edge(1, 2, 3),
        Edge(2, 1, 1)
      ), 1 to 2
    )
    assert(solution(graph, 1) === 1)
  }
  test("test 3") {
    val graph = Graph(
      Seq(
        Edge(1, 2, 2),
        Edge(1, 2, 3),
        Edge(2, 1, 1),
        Edge(3, 2, 14),
        Edge(2, 3, 12),
        Edge(3, 2, 10)
      ), 1 to 2
    )
    assert(solution(graph, 1) === 11)
  }
  test("test 4") {
    val graph = Graph(
      Seq(
        Edge(1, 2, 2),
        Edge(1, 5, 3),
        Edge(7, 8, 1),
        Edge(9, 7, 4),
        Edge(3, 4, 5),
        Edge(5, 2, 2),
        Edge(2, 8, 5),
        Edge(3, 9, 8),
        Edge(3, 7, 7)
      ), 1 to 9
    )
    println(
      mst(graph, 1, 0, Map(1 -> Result(1, 0, 0,emptyEdge))).toSeq.sortBy(_._1).mkString("\n")
    )
  }

  test("test 6") {
    val graph = Graph(
      Seq(Edge(1, 2, 1), Edge(2, 1, 2)), 1 to 2
    )
    assert(solution(graph, 1) === 1)
    assert(solution(graph, 2) === 1)
  }
  test("test 7") {
    val graph = Graph(
      Seq(
        Edge(1, 2, 1),
        Edge(2, 3, 2),
        Edge(3, 1, 3)

      )
      , 1 to 3
    )
    //    assert(solution(graph, 1) === 3)
    //    assert(solution(graph, 2) === 3)
    assert(solution(graph, 3) === 3)
  }

  test("test 8") {
    val graph = Graph(
      Seq(
        Edge(1, 2, 1),
        Edge(2, 3, 2),
        Edge(3, 4, 3),
        Edge(4, 1, 4)
      )
      , 1 to 4
    )
    assert(solution(graph, 1) === 6)
    assert(solution(graph, 2) === 6)
    //    assert(solution(graph, 3) === 6)
  }
  test("test 5") {
    val graph = Graph(
      Seq(
        Edge(1, 2, 100),
        Edge(2, 3, 200),
        Edge(3, 4, 300),
        Edge(4, 5, 400),
        Edge(5, 6, 500),
        Edge(6, 7, 600),
        Edge(7, 8, 700),
        Edge(8, 9, 800),
        Edge(9, 10, 900),
        Edge(10, 1, 1000)

      ), 1 to 10
    )

    println(
      mst(graph, 1, 0, Map(1 -> Result(1, 0, 0,emptyEdge))).toSeq.sortBy(_._1).mkString("\n")
    )

    println("-------------")

    println(
      mst(graph, 3, 0, Map(3 -> Result(3, 0, 0,emptyEdge))).toSeq.sortBy(_._1).mkString("\n")
    )
  }
  test("xxx") {
    val size = 10
    val r = new Random(16)
    val edges = 1 to size map { _ => Edge(abs(r.nextInt()) % size, abs(r.nextInt()) % size, abs(r.nextInt()) % 5000) }
    val nodes = 1 to size
    val graph = Graph(edges, nodes)
    println(
      mst(graph, 1, 0, Map(1 -> Result(1, 0, 0,emptyEdge))).toSeq.sortBy(_._1).mkString("\n")
    )

  }


}
