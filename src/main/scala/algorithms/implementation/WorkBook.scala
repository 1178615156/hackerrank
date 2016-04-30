package algorithms.implementation

import utils.SetInt


/**
  * Created by yuJieShui on 2016/4/24.
  */
object WorkBook {

  def toPage(list: List[Int],pageSize:Int): List[Seq[Int]] = {

    def chapter2page(chapter: Seq[Int], rt: Seq[Seq[Int]]): Seq[Seq[Int]] = {
      def result = chapter.take(pageSize) +: rt
      if (chapter.size > pageSize)
        chapter2page(chapter.drop(pageSize), result)
      else
        result.reverse
    }
    list.map(1 to _).flatMap(c => chapter2page(c, Nil))
  }

  def specialNum(page: Seq[Seq[Int]]): Int = {
    (1 to page.size) zip page count{case (index,value) => value.contains(index)}
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val n :: pageSize :: Nil = readListInt()
    val chapter = readListInt()
    val result = specialNum(toPage(chapter,pageSize))

    println(result)
  }


  SetInt(
    """5 3
      |4 2 6 1 10
    """.stripMargin
  )
}
