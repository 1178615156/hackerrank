package functionalProgramming.functionalStructures

import org.scalameter.Warmer
import org.scalatest.{FunSuite, WordSpecLike}

import scala.util.Random

/**
  * Created by yujieshui on 2017/1/23.
  */
class StockPredictionTest extends WordSpecLike {

  import StockPrediction._

  "solution" must {
    "case 1 " in {
      val result = solution(
        Seq(3, 5, 2, 6, 1),
        Seq(
          0L -> 2L,
          2L -> 3L
        )
      )
      println(result)
    }
  }
  "time" in {
    val random = new Random()
    val totalTime = utils.TimeUtil.VarTime()
    totalTime.computeTime{
      def element(): Long = math.abs(random.nextLong() % 1e9.toLong )
      val array = 1 to 50000 map (_ => element())
      val q = 1 to 10000 map (_ => (array.indices zip array minBy(_._2))._1.toLong -> Int.MaxValue.toLong)
      (solution(array, q))

    }
    println(totalTime)
  }
}
