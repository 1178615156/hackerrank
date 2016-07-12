package utils

/**
  * Created by yuJieShui on 2016/5/18.
  */
case class TestNextData(var l: List[String]) {
  def next() = {
    val r = l.head
    l = l.tail
    r.split(" ").toList
  }
}
