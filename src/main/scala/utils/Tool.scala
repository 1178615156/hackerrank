package utils

import scala.collection.immutable.IndexedSeq

/**
  * Created by yujieshui on 2016/3/14.
  */
object Tool {
  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def listIsSort(l: Seq[Int], now: Int = Int.MinValue): Boolean = {
    if (l.isEmpty)
      true
    else if (now > l.head)
      false
    else
      listIsSort(l.tail, l.head)
  }

  type Grid[X] = Seq[Array[X]]
  def readGrid[R](lineNum: Int): Grid[Char] = readGrid(lineNum,_.toArray)

  def readGrid[R](lineNum :Int,s: String => R ): Seq[R] ={
    1 to lineNum map (_ => s(io.StdIn.readLine()))
  }
  scala.annotation.elidable
  case class Paint[T](xx:Int,yy:Int ,value:Option[T] = None ){
    def row = yy
    def column = xx
  }
}
