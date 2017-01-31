package functionalProgramming.functionalStructures

import org.scalatest.{FunSuite, WordSpecLike}

/**
  * Created by yujieshui on 2017/1/26.
  */
class PrisonTransportTest extends WordSpecLike {

  import PrisonTransport._

  val map1 = Map(
    1 -> Seq(1, 2),
    2 -> Seq(1, 2),
    3 -> Seq(3)
  )

  "to group" must {
    "case 1 " in {
      println(to_group(1, map1, Set()))
    }
  }
  "spilt" must {
    "case 1 " in {
      println(spilt(3, map1, Set()).mkString("\n"))
    }
  }
  "solution" must {
    "case 1 " in {
      val result = solution(4,Seq(
        (1,2),
        (1,4)
      ))

      println(result)

    }
    "case 2 " in {
      val result = solution(6,Seq(
        (1,2),
        (2,3),
        (4,5)
      ))
      println(result)
    }
  }
}
