val s = "((. X (. . .)) . (X . (. X X)))".toList.filter(_ != ' ')

trait Term

case object On extends Term

case object Off extends Term

case class LifeTree(
                     left: Term,
                     node: Term,
                     fight: Term
                   ) extends Term


def parsing(list: List[Char],
            currentTerm: List[Term] = Nil
           ): (Term, List[Char]) = {
  list.head match {
    case '(' =>
      val (term, tail) = parsing(list.tail)
      if (tail.isEmpty)
        (term, tail)
      else
        parsing(tail, currentTerm :+ term)
    case ')' =>
      LifeTree(
        currentTerm(0),
        currentTerm(1),
        currentTerm(2)
      ) -> list.tail
    case 'X' =>
      parsing(list.tail, currentTerm :+ On)
    case '.' =>
      parsing(list.tail, currentTerm :+ Off)
  }
}

parsing(s)

