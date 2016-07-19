package algorithms.implementation

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable

import java.io.File

import utils.SetInt

/**
  * Created by yuJieShui on 2016/1/1.
  */
object AngryProfessor {
  SetInt(
    """
      |2
      |4 3
      |-1 -3 4 2
      |4 2
      |0 -1 2 1
    """.stripMargin
  )

  case class Data(studentNum: Int, cancelation: Int, arrivalTime: List[Int])

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val datas = 1 to n map (_ ⇒ {
      val student = sc.nextInt()
      val cancelation = sc.nextInt()
      val arrivalTime = 1 to student map (_ ⇒ sc.nextInt())
      Data(student, cancelation, arrivalTime.toList)
    })

    datas foreach {
      case Data(studentNum: Int, cancelation: Int, arrivalTime: List[Int]) ⇒
        val result = arrivalTime.count(_ <= 0) < cancelation
        println(if (result)
          "YES"
        else
          "NO"
        )
    }
  }
}


