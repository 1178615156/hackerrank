package algorithms.implementation

import org.scalatest.FunSuite

class AbsolutePermutationTest extends FunSuite {

  import AbsolutePermutation._

  def list(n: Int) = (1 to n).toList

  test("sample input") {
    //    assert(solution(2, 1) === 2 :: 1 :: Nil)
    assert(solution(3, 0) === 1 :: 2 :: 3 :: Nil)
    assert(solution(3, 2) === -1 :: Nil)
  }
  test("4 2") {
    assert(solution(4, 2) === 3 :: 4 :: 1 :: 2 :: Nil)

  }

  test("5") {
    //    assert(solution(5, 0) == List(1, 2, 3, 4, 5))
    //    assert(solution(5, 1) == List(2, 3, 4, 5, 1))
    assert(solution(5, 2) == List(3, 4, 5, 2, 3))

    //    1,2,3,4,5
    //    3,4,5,2,3
    assert(solution(5, 3) == List(-1))
    assert(solution(5, 4) == List(-1))
    assert(solution(5, 5) == List(-1))
  }
  test("100 2") {
    val l = "3 4 1 2 7 8 5 6 11 12 9 10 15 16 13 14 19 20 17 18 23 24 21 22 27 28 25 26 31 32 29 30 35 36 33 34 39 40 37 38 43 44 41 42 47 48 45 46 51 52 49 50 55 56 53 54 59 60 57 58 63 64 61 62 67 68 65 66 71 72 69 70 75 76 73 74 79 80 77 78 83 84 81 82 87 88 85 86 91 92 89 90 95 96 93 94 99 100 97 98"

    val result = (solution(100, 2))
    assert(
      result === l.split(" ").map(_.toInt).toList
    )
  }
  test("100 10") {
    val result = (solution(101, 11))
  }

  test("fail") {
    for {
      n <- 0 to(20, 2)
      k <- 1 to (n / 2)
    } yield
      println((n, k, solution(n, k) != List(-1), n / k % 2 == 0, n % k == 0))
  }
}
