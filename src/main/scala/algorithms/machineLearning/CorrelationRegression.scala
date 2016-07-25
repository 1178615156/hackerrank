package algorithms.machineLearning

/**
  * Created by yujieshui on 2016/7/25.
  */
object CorrelationRegression {

  def pearsonProduct(x: Seq[Double], y: Seq[Double]) = {
    val x_mean = x.sum / x.size
    val y_mean = y.sum / y.size
    val omg = (x.map(_ - x_mean) zip y.map(_ - y_mean) map { case (xx, yy) => xx * yy }).sum

    omg / (
      math.sqrt(x.map(_ - x_mean).map(e => e * e).sum)
        * math.sqrt(y.map(_ - y_mean).map(e => e * e).sum)
      )
  }

  def main(args: Array[String]): Unit = {
    println(
      pearsonProduct(
        x = Seq(15, 12, 8, 8, 7, 7, 7, 6, 5, 3),
        y = Seq(10, 25, 17, 11, 13, 17, 20, 13, 9, 15)
      )
    )
  }
}
