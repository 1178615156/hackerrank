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
    assert(paintGridAround(paint, grid).map(_.value.get) == "%%--".toList.reverse)
  }

  test("mkTree") {
    val begin: Paint[Char] = Paint(pacMain_column, pacMan_row)
    val end: Paint[Char] = Paint(food_column, food_row)
    val result = (mkTree(begin, end, grid))
    println(result)
  }

  test("mk tree test") {
    val begin: Paint[Char] = Paint(column = 1, row = 1)
    val end: Paint[Char] = Paint(column = 1, row = 3)
    val result = (mkTree(begin, end, grid))

    assert(result.value.forall(e => e.row == 1 && e.column == 1))

    assert(result.child.head.value.forall(e => e.row == 2 && e.column == 1))

    assert(result.child.head.child.head.value.forall(e => e.row == 3 && e.column == 1))
    println(result)
  }

  test("mk tree test 3,14 -> 2,17"){
    val begin: Paint[Char] = Paint(row = 3, column = 14)
    val end: Paint[Char] = Paint(row = 2, column = 17)
    val result = (mkTree(begin, end, grid))
    println(result)
  }

  test("dfs") {
    val begin: Paint[Char] = Paint(1, 1)
    val end: Paint[Char] = Paint(3, 1)
    val tree = (mkTree(begin, end, grid))

    val dfsPath = dfs(Seq(tree), end)

    println(dfsPath)
  }
}












