package functionFractal

/**
  * Created by yujieshui on 2016/3/15.
  */
object TreeLife {


  trait Term

  case object On extends Term

  case object Off extends Term

  case class LifeTree(
                       left: Term,
                       node: Term,
                       fight: Term
                     ) extends Term


  def parsing(list: List[Char]) = {
    def impl(list: List[Char],
             currentTerm: List[Term] = Nil
            ): (Term, List[Char]) = {
      list.head match {
        case '(' =>
          val (term, tail) = impl(list.tail)
          if (tail.isEmpty)
            (term, tail)
          else
            impl(tail, currentTerm :+ term)
        case ')' =>
          LifeTree(
            currentTerm(0),
            currentTerm(1),
            currentTerm(2)
          ) -> list.tail
        case 'X' =>
          impl(list.tail, currentTerm :+ On)
        case '.' =>
          impl(list.tail, currentTerm :+ Off)
      }
    }

    impl(list)._1
  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val ruleNum :: Nil = readListInt()
    val cellTreeString = io.StdIn.readLine().filter(_ != ' ')
  }
}
