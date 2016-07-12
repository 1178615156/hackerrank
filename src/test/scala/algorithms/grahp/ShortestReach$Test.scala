package algorithms.grahp

import java.io.File

import org.scalatest.FunSuite
import Entity._
import utils.TestNextData

import scala.io.Source


class ShortestReach$Test extends FunSuite {

  import ShortestReach._

  val t1Data = TestNextData(
    List(
      "1"
      , "4 4"
      , "1 2 24"
      , "1 4 20"
      , "3 1 3"
      , "4 3 12"
      , "1"
    )
  )
  test("t1") {

    val result = readIn(() => t1Data.next.map(_.toInt)) map {
      case (startNode, nodeSize, edgeList) =>
        solution(startNode, Grahp(edgeList, 1 to nodeSize map (e => Node(e))))
    }
    assert(
      result.map(_.map(_.distances).mkString(" ")).mkString("\n") === "24 3 15"
    )
  }

  val t2Data =
    TestNextData(Source.fromFile(new File("D:\\git\\hackerrank\\src\\main\\resources\\grahp_ShortestReach")).getLines().toList)
  test("t2") {

    val result = readIn(() => t2Data.next.map(_.toInt)) map {
      case (startNode, nodeSize, edgeList) =>
        solution(startNode, Grahp(edgeList, 1 to nodeSize map (e => Node(e))))
    }

    assert(
      result.map(_.map(_.distances).mkString(" ")).mkString("\n") ===
        "3 6 4 5 5 4 5 4 3 3 4 6 6 4 4 4 4 5 3 4 5 3 4 6 8 4 5 3 4 4 5 4 6 6 2 4 6 4 4 4 4 5 5 3 4 5 3 6 5 4 5 5 4 4 5 3 3 4 2 3 5 2 4 4 3 4 10 5 5 7 4 4 4 1 4 4 4 5 4 4 5 4 4 5 4 5 6 5 4 4 5 5 5 4 4 4 4 3 4 5 3 3 5 4 6 8 2 5 3 4 4 5 3 5 3 3 4 5 3 6 5"
    )


  }

  test("case 2 ") {
    val caseT2Data = TestNextData(
      Source.fromFile(ClassLoader.getSystemResource("grahp_ShortestReach_2.txt").getFile).getLines().toList
    )
    val result = readIn(() => caseT2Data.next().map(_.toInt)).map {
      case (startNode, nodeSize, edgeList) =>
        solution(startNode, Grahp(edgeList, 1 to nodeSize map (e => Node(e))))
    }

    val gold =
      List("20 25 25 68 86 39 22 70 36 53 91 35 88 27 30 43 54 74 41",
        "9 8 8 8 12 7 15 8 4 1 12 9 7 10 4 10 10 4 1 7 12 7 11 12 15 10 5 11 6 7 9 11 9 7 7 14 5 13 6 8 10 7 4 9 3 5 5 9 13 1 8 11 4 9 6 7 7 8 11 6 10 7 8 9 13 9 12 8 3 5 7 15 6 10 11 5 11",
        "154 90 186 190 178 114 123 -1 -1 123 -1 104 -1 -1 -1 207 134 123 98 155 -1 198 68 90 170 135 -1 103 145 -1 54 111 163 173 115 87 159 75 -1 94 102 -1 76 67 167 138 216 -1 172 102 212 163 103 112 -1 182 49 145 92 -1 -1 194 -1 182 -1 201 96 -1 85 121 108 161 130 100 120 -1 -1 118 215 92 156 162 163 168 71 110 -1 -1 190 217 100 105 178",
        "13 30 17 33 16 9 31 34 14 20 21 19 24 34 27 42 15 16 19 23 18 21 11 21 28 15 15 45 18 26 17 20 16 28 27 16 22 21 18 21 34 14 26 27 11 23 17 24 27 22 19 18 21 17 17 22 14 20 12 27 21 10 42 10 25 19 22",
        "3 6 8 11 7 12 10 18 4 8 3 6 12 1 2 10 1 8 5 6 9 9 8 17 11 12 8",
        "3 4 5 3 4 5 5 4 4 7 6 4 1 4 5 5 5 4 5 6 5 6 4 5 3 5 5 6 2 6 3 3 6 5 3 6 3 2 6 4 1 6 3 4 5 6 7 7 3 6 3 5 3 5 4 7 4 4 6 4 5 5 5 4 2 2 3 6 4 6 4 4 5 4 6 3 5 5 4 4 4 2 1 3 3 3 2"
      )
    gold zip result.map(_.map(_.distances).mkString(" ")) foreach {
      case (g, r: String) => assert(g === r)
    }
    println(
      result.map(_.map(_.distances).mkString(" ")).mkString("\n")
    )
  }


}
