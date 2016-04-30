package algorithms.implementation

/**
  * Created by yuJieShui on 2016/4/24.
  */
object GridSearch {
//  val s = scala.io.Source.fromFile(new File("F:\\迅雷下载\\input05.txt")).getLines().mkString("\n")
//
//  SetInt(s)
Nil.containsSlice(Nil)
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
