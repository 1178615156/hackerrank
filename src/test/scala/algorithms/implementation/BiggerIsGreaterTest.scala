package algorithms.implementation

import org.scalatest.FunSuite

import scala.language.implicitConversions

/**
  * Created by yujieshui on 2017/5/11.
  */
class BiggerIsGreaterTest extends FunSuite {

  import BiggerIsGreater._

  implicit def s2l(s: String): List[Char] = Predef.augmentString(s).toList

  test("a") {
    assert(solution("ab") === Some(s2l("ba")))

    assert(solution("bb") === None)
    assert(solution("hefg") === Some(s2l("hegf")))
    assert(solution("dhck") === Some(s2l("dhkc")))
    assert(solution("dkhc") === Some(s2l("hcdk")))
  }
  test("n") {
    println(
      solution("a")
    )
  }
  test("opt") {
    val x = 1 to 100000 map { i =>
      solution(1 to 100 map (_ => (math.random * 255).toInt.toChar) toList)
    }
//    val o = x.mkString("\n")
//    Thread.sleep(10000)
//    println(o)
//    Thread.sleep(10000)

  }
}
