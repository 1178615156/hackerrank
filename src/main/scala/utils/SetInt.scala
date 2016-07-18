package utils

import java.io.ByteArrayInputStream

/**
  * Created by yuJieShui on 2016/1/1.
  */
object SetInt {
  def apply(s: String): Unit = {
    System.setIn(new ByteArrayInputStream(
      s.getBytes
    ))
  }

  case class VarTime(var time: Long = 0) {
    def set(f: Long => Long) = time = f(time)
  }

  def computeTime[T](t: => T)(implicit varTime: VarTime) = {
    val startTime = System.currentTimeMillis()
    val result = t
    val endTime = System.currentTimeMillis()

    varTime.set(_ + (endTime - startTime))
    result
  }
}
