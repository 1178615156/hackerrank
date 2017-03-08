package functionalProgramming.memoization

import scala.reflect.ClassTag

/**
  * Created by yujieshui on 2017/3/7.
  */
object PasswordCracherFP {

  def parser(words: Seq[String], string: String,rt:Seq[String]=Seq()): Seq[Seq[String]] = {
    if(string.isEmpty) Stream(rt.reverse)
    else
      words
        .filter(word => word == string.take(word.length))
        .flatMap(word => parser(words, string.drop(word.length),word +: rt ))
  }

  def solution(words: Seq[String], string: String): String = {
    val result = parser(words.toStream, string).filter(_.mkString("") == string)
    if(result.isEmpty)
      "WRONG PASSWORD"
    else
      result.head.mkString(" ")
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val data = 1 to n map { _ =>
      val m :: Nil = readListInt()
      val words = io.StdIn.readLine().split(" ")
      val passAccept = io.StdIn.readLine()
      words -> passAccept
    }
    val result = data.map { case (words, passAccept) => solution(words, passAccept) }

    println(result.mkString("\n"))
  }
}
