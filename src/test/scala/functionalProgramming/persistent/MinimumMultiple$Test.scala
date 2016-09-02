package functionalProgramming.persistent

import java.io.File

import org.scalatest.FunSuite
import utils.NextLine

import scala.io.Source

/**
  * Created by yujieshui on 2016/8/29.
  */
class MinimumMultiple$Test extends FunSuite {

  import MinimumMultiple._

  test("gcd") {
    assert(gcd(1, 1) === 1)
    assert(gcd(2, 1) === 1)
    assert(gcd(3, 1) === 1)

    assert(gcd(1, 1) === 1)
    assert(gcd(1, 2) === 1)
    assert(gcd(1, 3) === 1)

    assert(gcd(2, 3) === 1)
    assert(gcd(3, 2) === 1)
    assert(gcd(2, 5) === 1)
    assert(gcd(5, 1) === 1)
    assert(gcd(5, 1) === 1)
    assert(gcd(5, 1) === 1)

    assert(gcd(3, 3) === 3)
    assert(gcd(10, 10) === 10)

    assert(gcd(10, 5) === 5)
    assert(gcd(5, 10) === 5)
    assert(gcd(1000, 200) === 200)
    assert(gcd(90, 60) === 30)
  }

  test("minimumMultiple") {
    assert(minimumMultiple(Seq(1, 2, 3)) === 6)
    assert(minimumMultiple(Seq(1, 1, 1)) === 1)
    assert(minimumMultiple(Seq(1, 2, 3, 4)) === 12)
    assert(minimumMultiple(Seq(2, 10, 6, 1, 9)) === 90)
    assert(minimumMultiple(Seq(6, 8)) === 24)
  }

  def file(s: String): File = new File(this.getClass.getClassLoader.getResource(s).getFile)

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
      s"""minimumMultipleTime : ${minimumMultipleTime} -- ${minimumMultipleTime.t.toDouble / time.value}
          |all time ${time}
      """.stripMargin)
  }
}

