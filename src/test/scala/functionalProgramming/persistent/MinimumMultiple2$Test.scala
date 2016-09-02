package functionalProgramming.persistent

import java.io.File

import org.scalatest.FunSuite
import utils.NextLine

import scala.io.Source

/**
  * Created by yujieshui on 2016/9/2.
  */
class MinimumMultiple2$Test extends FunSuite {
  def file(s: String): File = new File(this.getClass.getClassLoader.getResource(s).getFile)

  import MinimumMultiple2._

  test("0") {

    val (initArr, actions) = read(NextLine.fromSeq(Seq(
      "5",
      "2 5 6 1 9",
      "7",
      "Q 0 4",
      "U 1 2",
      "Q 0 2",
      "Q 3 4",
      "Q 2 4",
      "U 3 8",
      "Q 2 3"
    )))
    val out = solution(actions, mkArr(initArr))(Seq())
    val expect = Seq(
      "90",
      "30",
      "9",
      "18",
      "24"
    ).map(_.toInt)
    assert(out === expect)
  }
  test("test case 3") {
    val (initArr, actions) = read(NextLine.fromFile(file("fp_minimum-multiple-5")))

    val expect = Source.fromFile(file("fp_minimum-multiple-5-expected")).getLines().toSeq.map(_.toInt)
    import org.scalameter._
    import Key._
    val time = config().withWarmer(Warmer.Zero).measure {
      val out = solution(actions, mkArr(initArr))(Seq()) map (_ % M)
      assert(out === expect)
    }
    println(
      s"""
        |all time $time
      """.stripMargin)
  }
}
