package utils

import java.io.ByteArrayInputStream

/**
  * Created by yuJieShui on 2016/1/1.
  */
object TimeUtil{
  case class VarTime(var time: Long = 0) {
    def set(f: Long => Long) = time = f(time)

    def computeTime[T](t: => T) = {
      val startTime = System.currentTimeMillis()
      val result = t
      val endTime = System.currentTimeMillis()
      this.set(_ + (endTime - startTime))
      result
    }
    override def toString: String = {
      s"${time.toDouble/1000} s"
    }

  }


}
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
object ReadSourceFile{
  def apply(s: String) = this.getClass.getClassLoader.getResource(s).getFile

}