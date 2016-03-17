package ai

import scala.collection.immutable.IndexedSeq

import org.scalatest.FunSuite
import utils.SetInt
import utils.Tool._

/**
  * Created by yuJieShui on 2016/3/17.
  */
class TestIo(var source: Seq[String]) {
  def readListInt() = {
    val rt = source.head.split(" ").toList.map(_.toInt)
    source = source.drop(1)
    rt
  }

  def readGrid[R](lineNum: Int): Grid[Char] = readGrid(lineNum, _.toArray)

  def readGrid[R](lineNum: Int, s: String => R): Seq[R] = {
    val rt = source.take(lineNum).toList.map(s)
    source = source.drop(lineNum)
    rt
  }
}

class PacManDFSTest extends FunSuite {

  import PacManDFS._


  var source =
    """3 9
      |5 1
      |7 20
      |%%%%%%%%%%%%%%%%%%%%
      |%--------------%---%
      |%-%%-%%-%%-%%-%%-%-%
      |%--------P-------%-%
      |%%%%%%%%%%%%%%%%%%-%
      |%.-----------------%
      |%%%%%%%%%%%%%%%%%%%%
    """.stripMargin.split("\r\n")
  val tio    = new TestIo(source)

  import tio._

  val pacMan_row :: pacMain_column :: Nil = readListInt()
  val food_row :: food_column :: Nil      = readListInt()
  val grid_row :: grid_column :: Nil      = readListInt()
  val grid                                = readGrid(grid_row)


  test("paintGridAround") {
    val paint: Paint[Char] = Paint(1, 1)
    assert(paintGridAround(paint, grid) == "%%--".toList.reverse)
  }
}












