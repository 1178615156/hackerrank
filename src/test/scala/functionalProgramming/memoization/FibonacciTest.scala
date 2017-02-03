package functionalProgramming.memoization

import org.scalatest.{FunSuite, WordSpecLike}

/**
  * Created by yujieshui on 2017/2/2.
  */
class FibonacciTest extends WordSpecLike {
  import Fibonacci._

  "aa" in {
    println(
      solution(Seq(0,1,5,10,100))
    )
  }

}
