package contests.worldCodeSpring

import contests.worldCodeSpring.EqualStack.{Stack, solution}
import org.scalatest.FunSuite

/**
  * Created by yuJieShui on 2016/6/26.
  */
class EqualStackTest extends FunSuite {
  test("solution") {
    assert(solution(
      Seq(
        Seq(3, 2, 1, 1, 1),
        Seq(4, 3, 2),
        Seq(1, 1, 4, 1)
      ).map(seq => Stack(seq,seq.sum ))
    ) === 5)

    assert(solution(Seq(Seq[Int]()).map(seq => Stack(seq,seq.sum ))) === 0 )
  }
}
