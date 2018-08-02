package algorithms.strings

import scala.util.Try


object BuildPalindrome {


  type Value = List[Char]

  def enumImpl(a: List[Char], b: List[Char]): Set[String] = {
    if(a.isEmpty || b.isEmpty) Set()
    else {
      enumImplIterB(a, b).toSet ++ enumImpl(a.tail, b)
    }

  }

  def enumImplIterB(a: List[Char], b: List[Char]): List[String] = {
    if(a.isEmpty || b.isEmpty) Nil
    else {
      val char = a.head
      val b_tail = b.drop(b.indexOf(char))
      if(b_tail.size == b.size) Nil
      else {
        val result = countSame(a, b_tail).mkString
        result +: enumImplIterB(a, b_tail.tail)

      }

    }
  }

  def countSame(a: List[Char], b: List[Char]): Value = {
    if(b.isEmpty && a.isEmpty) Nil
    else if(a.isEmpty && b.nonEmpty) b.head :: Nil
    else if(a.nonEmpty && b.isEmpty) a.head :: Nil
    else if(a.head != b.head) math.min(a.head, b.head).toChar :: Nil
    else a.head +: countSame(a.tail, b.tail) :+ b.head

  }

  def buildPalindrome(a: String, b: String): String = {
    Try(enumImpl(a.toList, b.reverse.toList).maxBy(_.length)).getOrElse("-1")
  }

  def main(args: Array[String]): Unit = {

  }
}
