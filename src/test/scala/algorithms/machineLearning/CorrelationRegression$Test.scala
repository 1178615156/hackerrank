package algorithms.machineLearning

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2016/7/25.
  */
class CorrelationRegression$Test extends FunSuite {

  import CorrelationRegression._

  test("x") {
    val result = pearsonProduct(
      Seq(1, 2, 3, 5, 8),
      Seq(0.11, 0.12, 0.13, 0.15, 0.18)
    )

    assert(
      result === 1
    )
  }
}
