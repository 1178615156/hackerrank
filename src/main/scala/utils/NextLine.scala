package utils

import java.io.{BufferedReader, File, InputStreamReader}

import scala.io.Source

/**
  * Created by yujieshui on 2016/8/30.
  */


object NextLine {
  type NextLine = () => String

  def fromSystemIn(): NextLine = {
    val bi = new BufferedReader(new InputStreamReader(System.in))
    () => bi.readLine()
  }

  def fromFile(file: File): NextLine = {
    fromSeq(Source.fromFile(file).getLines().toSeq)
  }

  def fromSeq(seq: Seq[String]): NextLine = {
    var list = seq
    () => {
      val r = list.head
      list = list.tail
      r
    }
  }
}