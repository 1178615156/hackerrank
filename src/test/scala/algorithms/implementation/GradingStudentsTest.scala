package algorithms.implementation

import org.scalatest.FunSuite

/**
  * Created by yujieshui on 2017/5/9.
  */
class GradingStudentsTest extends FunSuite {
  test("solution"){
    0 to 100 foreach {i=>

      println(GradingStudents.solution(Seq(i)))
    }
  }
}
