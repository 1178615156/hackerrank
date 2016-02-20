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
}
