package utils

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
}
