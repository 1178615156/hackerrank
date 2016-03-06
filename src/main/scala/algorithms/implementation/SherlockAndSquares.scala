package algorithms.implementation

import utils.SetInt

/**
  * Created by yuJieShui on 2016/3/5.
  */
object SherlockAndSquares {

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  type Interval = Range

  def squares(interval: Interval) = {
    val begin = math.sqrt(interval.start).toInt
    val end = math.sqrt(interval.end).toInt
    begin to end map (e => e * e) count interval.contains
  }

  def main(args: Array[String]) {
    val n :: Nil = readListInt()
    val data = 1 to n map (_ => {
      val begin :: end :: Nil = readListInt()
      begin to end
    })

    val result = data map squares
    val out = result mkString "\n"
    println(out)
  }


  SetInt(
    """2
      |1 1
      |89784519 103811134
    """.stripMargin)
}
