package functionalProgramming.introduction

import org.scalatest.FunSuite

import scala.math.Pi

/**
  * Created by yujieshui on 2016/7/18.
  */
class ConcavePolygon$Test extends FunSuite {

  import ConcavePolygon._

  implicit class `With~=`(val double: Double) {
    def ~=(double: Double) =
      math.abs(double / this.double - 1) < 0.00001 || math.abs(double - this.double) < 0.00001
  }

  test("paint") {
    assert(Paint(0, 0).angle    ~= 0)
    assert(Paint(100, 0).angle  ~= 0)
    assert(Paint(1, 1).angle    ~= Pi / 4)
    assert(Paint(0, 1).angle    ~= Pi / 2)
    assert(Paint(-1, 1).angle   ~= Pi / 2 + Pi / 4)
    assert(Paint(-1, 0).angle   ~= Pi)
    assert(Paint(-1, -1).angle  ~= Pi + Pi / 4)
    assert(Paint(0, -1).angle   ~= Pi + Pi / 2)
    assert(Paint(1, -1).angle   ~= Pi + Pi / 2 + Pi / 4)
  }
  test("angle 1") {
    val angle = PaintDirection(Paint(-1, 0), Paint(0, 0), Paint(1, 0))
    assert(angle.angle === Pi)

  }
  test("angle 2") {
    val angle = PaintDirection(Paint(0, 1), Paint(0, 0), Paint(1, 0))
    assert(angle.angle === (Pi / 2))
  }
  test("angle 3") {
    val angle = PaintDirection(Paint(1, 2), Paint(1, 1), Paint(2, 1))
    assert(angle.angle === (Pi / 2))
  }
  test("angle 4") {
    assert(PaintDirection(Paint(-1, -1), Paint(0, 0), Paint(1, 1)).angle === (Pi))
    assert(PaintDirection(Paint(-1, -1), Paint(0, 0), Paint(1, 0)).angle === (Pi + Pi / 4))
    assert(PaintDirection(Paint(-1, -1), Paint(0, 0), Paint(1, -1)).angle === (Pi / 2))
  }



  test("solution 1") {
    assert(
      solution(
        List(
          Paint(622, 991),
          Paint(1054, 665),
          Paint(661, 485)
        )
      ) === false
    )

  }


  test("solution 2") {
    assert(
      solution(
        List(
          Paint(0, 0),
          Paint(0, 1),
          Paint(1, 1),
          Paint(1, 0)
        )
      ) === false
    )
  }

  test("solution 3") {
    assert(
      solution(List(
        Paint(1028, 625),
        Paint(1042, 943),
        Paint(793, 1042),
        Paint(404, 909),
        Paint(574, 474),
        Paint(1077, 721),
        Paint(392, 543),
        Paint(572, 1005),
        Paint(963, 1020),
        Paint(857, 390)
      )) === false
    )
  }
}
