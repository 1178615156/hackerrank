package mathematics.probability

import org.scalatest.{FunSuite, WordSpecLike}

/**
  * Created by yujieshui on 2017/1/23.
  */
class SherlockAndProbabilityTest extends WordSpecLike {

  import SherlockAndProbability._

  "solution" must {
    "1011" in {
      val array =
        "1011".toList.map {
          case '1' => true
          case '0' => false
        }.toArray
      println(

        solution(
          array,
          Seq(3, 1)
        )
      )
    }
  }

}
