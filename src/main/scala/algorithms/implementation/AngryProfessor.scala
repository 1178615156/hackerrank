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
    val sc = new java.util.Scanner(System.in);
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

object GridSearch {
  val s = scala.io.Source.fromFile(new File("F:\\迅雷下载\\input05.txt")).getLines().mkString("\n")

  SetInt(s)

  import scala.collection.immutable.IndexedSeq
  import scala.collection.mutable

  var subMatrixTime = 0l
  var eqTime        = 0l

  def subMatrix[T](in: Seq[Seq[T]], n: Int): Seq[Seq[Seq[T]]] = {
    val bt = System.currentTimeMillis()
    val endIndex = n to in.size
    val beginIndex = endIndex.indices
    val rt = (beginIndex zip endIndex) map {
      case (b, e) ⇒ in.slice(b, e)
    }
    subMatrixTime += System.currentTimeMillis() - bt + 1
    rt
  }

  def eqs[T](in: Seq[Seq[T]], out: Seq[Seq[T]]) = {
    val bt = System.currentTimeMillis()

    val rt = in == out
    eqTime += System.currentTimeMillis() - bt
    rt
  }


  def run[T](in: Seq[Seq[T]], out: Seq[Seq[T]]): Boolean = {
    val outTranspose = out.transpose
    val outTransposeSize = outTranspose.size
    val rt = subMatrix(in, out.size) exists (in ⇒ {
      subMatrix(in.transpose, outTransposeSize) exists (e ⇒ eqs(e, outTranspose))
    })
    rt
  }


  def main(args: Array[String]) {
    case class Matrix[T](rowNum: Int, colunmNum: Int, matrix: Seq[Seq[T]])

    case class Data[T](in: Matrix[T], out: Matrix[T])
    val sc = new java.util.Scanner(System.in)

    def readMatrix = {
      val rowNum = sc.nextInt()
      val colunmNum = sc.nextInt()
      sc.nextLine()

      val matrix = (1 to rowNum) map { _ ⇒ mutable.ArraySeq(sc.nextLine(): _*) }
      Matrix(rowNum, colunmNum, matrix)

    }
    val t = sc.nextInt()
    val datas = 1 to t map (_ ⇒ {
      Data(readMatrix, readMatrix)
    })
    val tb = System.currentTimeMillis()
    val result = datas.map(e ⇒ run(e.in.matrix, e.out.matrix))
    //    if (false)
    println(
      s"""
         |all time   :${System.currentTimeMillis() - tb}
         |sbt matrix :$subMatrixTime
         |eq time    :$eqTime
       """.stripMargin)
    println(result map (if (_) "YES" else "NO") mkString "\n")
  }
}
