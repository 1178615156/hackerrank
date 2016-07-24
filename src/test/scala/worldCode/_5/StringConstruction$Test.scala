package worldCode._5

import org.scalatest.FunSuite

import scala.util.Random

//import org.scalameter.api._

/**
  * Created by yuJieShui on 2016/7/24.
  */
class StringConstruction$Test extends FunSuite {

  import StringConstruction._
  import org.scalameter._


  test("1") {
    assert(solution(StringBuilder.newBuilder append "abcd") === 4)
    assert(solution(StringBuilder.newBuilder append "abab") === 2)
  }

  test("time") {
    val r = new Random(16)
    val measurer = new Measurer.Default
    val st = System.currentTimeMillis()

    val time = measure {
      solution(StringBuilder.newBuilder append (1 to 10000 map (_ => r.nextInt(128))))
    }
    println(s"time :${time}")
  }
}
