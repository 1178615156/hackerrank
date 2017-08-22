package algorithms.recursion

import scala.collection.mutable

object PasswordCracker {

  def impl(pass: Seq[String], loginAttempt: String, cache: mutable.Map[String, Option[Seq[String]]]): Option[Seq[String]] = {
    if(loginAttempt.isEmpty)
      Some(Nil)
    else if(cache.contains(loginAttempt))
      cache(loginAttempt)
    else {
      val result =
        pass
          .filter(s => loginAttempt.startsWith(s))
          .map(s => impl(pass, loginAttempt.drop(s.length), cache).map(s +: _))
          .collect { case Some(x) => x }
          .headOption
      cache += (loginAttempt -> result)
      result
    }
  }

  def solution(pass: Seq[String], loginAttempt: String): Option[Seq[String]] = {
    val loginAttemptSet = loginAttempt.toSet

    if((loginAttemptSet diff pass.mkString("").toSet).nonEmpty)
      None
    else
      impl(
        pass.toStream,
        loginAttempt,
        mutable.Map[String, Option[Seq[String]]]())
  }

  def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)

  def main(args: Array[String]): Unit = {
    val n :: Nil = readListInt()
    val inData = 1 to n map { _ =>
      io.StdIn.readLine()
      val pass = io.StdIn.readLine().split(" ").toSeq
      val loginAttempt = io.StdIn.readLine()
      (pass, loginAttempt)
    }
    val result = inData.map { case (pass, la) => solution(pass, la) }
    println(result
      .map(_.map(_.mkString(" ")).getOrElse("WRONG PASSWORD"))
      .mkString("\n")
    )
  }

}
