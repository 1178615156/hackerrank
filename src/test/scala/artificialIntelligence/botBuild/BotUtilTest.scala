package artificialIntelligence.botBuild

import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/1/2.
  */
class BotUtilTest extends FunSuite {

  import BotUtil._

  test("test min skip 1") {
    val out = minSkip(Point(0, 0), Seq(Point(0, 1), Point(1, 0), Point(1, 1)))
//    assert(out == List((Point(0, 1), 1), (Point(1, 0), 1), (Point(1, 1), 2)))
    println(out)
  }

  test("test min skip") {
    val in =
      """
        |d---d
        |-d---
        |--dd-
        |--dd-
        |dd--d
      """.stripMargin.split("\n").tail

    val dirty = find('d', in.toSeq.map(e => e: Seq[Char]))
    println(minSkip(Point(0, 0), dirty))
  }
  test("test min skip 2") {
    val in =
      """
        |----d
        |-----
        |-----
        |---b-
        |dd--d
      """.stripMargin.split("\n").tail
    val dirty = find('d', in.toSeq.map(e => e: Seq[Char]))
    println(minSkip(Point(3, 3), dirty))
  }

  test("testMoveStep") {

  }

  test("testMain") {

  }

  test("testFind") {
    val in =
      """
        |0 0
        |ddddd
        |d----
        |d----
        |d----
        |d---d
      """.stripMargin.split("\n").tail.tail
    val inData = find('d', in.toSeq.map(e => e: Seq[Char]))
    assert(inData ==
      Vector(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3), Point(0, 4), Point(1, 0), Point(2, 0), Point(3, 0), Point(4, 0), Point(4, 4))
    )
    println(inData)
  }

}
