package functionalProgramming.persistent

import org.scalatest.FunSuite

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

  }

  test("minimumMultiple") {
    assert(minimumMultiple(Seq(1, 2, 3)) === 6)
    assert(minimumMultiple(Seq(1, 1, 1)) === 1)
    assert(minimumMultiple(Seq(1, 2, 3, 4)) === 12)
    assert(minimumMultiple(Seq(2, 10, 6, 1, 9)) === 90)
    assert(minimumMultiple(Seq(6, 8)) === 24)
  }
}
