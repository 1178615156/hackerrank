package algorithms.grahp

import org.scalatest.FunSuite

import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Range.Inclusive
import scala.util.Random

/**
  * Created by yujieshui on 2016/7/15.
  */
class JourneyToTheMoon$Test extends FunSuite {

  import JourneyToTheMoon._

  val testData_1 = new {
    val graph     : Graph[Astronaut]   = Graph(Seq(Edge(0, 1), Edge(2, 3)), 0 to 3)
    val astronauts: Country[Astronaut] = (0 to 3).toSet
  }

  test("same country") {
    assert(
      sameCountry(0, testData_1.graph).toSet === Set(0, 1)
    )
    assert(
      sameCountry(1, testData_1.graph).toSet === Set(0, 1)
    )
    assert(
      sameCountry(2, testData_1.graph).toSet === Set(2, 3)
    )
    assert(
      sameCountry(3, testData_1.graph).toSet === Set(2, 3)
    )
  }


  test("different country astronaut") {
    assert(
      differentCountryAstronaut(testData_1.graph, testData_1.astronauts).toSet.map((e: Country[Astronaut]) => e.toSet) ===
        Set(Set(0, 1), Set(2, 3))
    )
  }


  test("combination") {
    assert(
      combination(Seq(Seq(1), Seq(2)).map(_.toSet)) === 1
    )
    assert(
      combination(Seq(0 to 1, 2 to 3).map(_.toSet)) === 4
    )
    assert(
      combination(Seq(1 to 3, 4 to 6, 7 to 9).map(_.toSet)) === 3 * 3 * 3
    )
    //    val r = new Random()
    //    val data: IndexedSeq[Inclusive] = 1 to 100000 map (_ =>1 to (math.abs(r.nextInt()) + 3))
    //    val startTime = System.currentTimeMillis()
    //    val x = combination(data)
    //    val end = System.currentTimeMillis()
    //    println(end - startTime)
  }

  test("solution") {
    assert(solution(
      testData_1.graph, testData_1.astronauts.toSeq
    ) === 4)
  }

  def time[T](t: => T): Long = {
    val startTime = System.currentTimeMillis()
    t
    val endTime = System.currentTimeMillis()
    endTime - startTime
  }

  test("mesure 1") {
    val size = 4
    val astronauts: Country[Astronaut] = (0 to size ).toSet
    val r = new Random(16)
    val edges = 1 to size map { _ => Edge(math.abs(r.nextInt()) % size, math.abs(r.nextInt()) % size) }

    val graph: Graph[Astronaut] = Graph(edges, astronauts.toSeq)


    println(s"diff country time : ${time(differentCountryAstronaut(graph, astronauts))}")
    println(s"all time ${time(solution(graph, astronauts.toSeq))}")

  }


  test("mesure 2") {
    val size = 100
    val astronauts: Country[Astronaut] = (0 to size ).toSet
    val r = new Random(16)
    val edges = 1 to size map { _ => Edge(math.abs(r.nextInt()) % size, math.abs(r.nextInt()) % size) }

    val graph: Graph[Astronaut] = Graph(edges, astronauts.toSeq)


    println(s"diff country time : ${time(differentCountryAstronaut(graph, astronauts))}")
    println(s"all time ${time(solution(graph, astronauts.toSeq))}")

  }

  test("mesure 3") {
    val size = 111
    val astronauts: Country[Astronaut] = (0 to size ).toSet
    val r = new Random(16)
    val edges = 1 to size map { _ => Edge(math.abs(r.nextInt()) % size, math.abs(r.nextInt()) % size) }

    val graph: Graph[Astronaut] = Graph(edges, astronauts.toSeq)


    println(s"diff country time : ${time(differentCountryAstronaut(graph, astronauts))}")
    println(s"all time ${time(solution(graph, astronauts.toSeq))}")

  }

  test("mesure 4") {
    val size  = 1000
    val astronauts: Country[Astronaut] = (0 to size ).toSet
    val r = new Random(16)
    val edges = 1 to size map { _ => Edge(math.abs(r.nextInt()) % size, math.abs(r.nextInt()) % size) }

    val graph: Graph[Astronaut] = Graph(edges, astronauts.toSeq)


    println(s"diff country time : ${time(differentCountryAstronaut(graph, astronauts))}")
    println(s"all time ${time(solution(graph, astronauts.toSeq))}")

  }
  test("mesure 5") {
    val size = 4000
    val astronauts: Country[Astronaut] = (0 to size ).toSet
    val r = new Random(16)
    val edges = 1 to size map { _ => Edge(math.abs(r.nextInt()) % size, math.abs(r.nextInt()) % size) }

    val graph: Graph[Astronaut] = Graph(edges, astronauts.toSeq)


    println(s"all time ${time(solution(graph, astronauts.toSeq))}")

  }


  test("mesure max") {
    val size = 100000
    val astronauts: Country[Astronaut] = (0 to size ).toSet
    val r = new Random(16)
    val edges = 1 to 10000 map { _ => Edge(math.abs(r.nextInt()) % size, math.abs(r.nextInt()) % size) }

    val graph: Graph[Astronaut] = Graph(edges, astronauts.toSeq)


    println(s"all time ${time(solution(graph, astronauts.toSeq))}")

  }
  test("test 11"){
    val size = 100000
    val astronauts: Country[Astronaut] = (0 to size ).toSet
    val r = new Random(16)
    val edges = Seq(Edge(1,2),Edge(3,4))

    val graph: Graph[Astronaut] = Graph(edges, astronauts.toSeq)
    println(solution(graph, astronauts.toSeq))
    println(s"all time ${time(solution(graph, astronauts.toSeq))}")

  }
}
