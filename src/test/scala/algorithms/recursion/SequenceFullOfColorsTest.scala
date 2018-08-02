package algorithms.recursion

import org.scalatest.FunSuite
import SequenceFullOfColors._

class SequenceFullOfColorsTest extends FunSuite {
  test("RGGR") {
    assert(solution("RGGR".toList.map(_.toString).map(Color.withName)))
  }

  test("RYBG") {
    assert(solution("RYBG".toList.map(_.toString).map(Color.withName)))
  }

  test("RYRB") {
    assert(!solution("RYRB".toList.map(_.toString).map(Color.withName)))
  }

  test("YGYGRBRB") {
    assert(!solution("YGYGRBRB".toList.map(_.toString).map(Color.withName)))
  }

}
