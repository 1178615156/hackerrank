val s = "((. X (. . .)) . (X . (. X X)))".toList.filter(_ != ' ')

trait Term

trait CellTerm extends Term

case object On extends CellTerm

case object Off extends CellTerm

case class LifeTree(
                     left: Option[LifeTree] = None,
                     node: CellTerm,
                     right: Option[LifeTree] = None,
                     father: Option[CellTerm] = None
                   ) extends Term


def parsing(list: List[Char],
            currentTerm: List[LifeTree] = Nil
           ): (LifeTree, List[Char]) = {
  list.head match {
    case '(' =>
      val (term, tail) = parsing(list.tail)
      if (tail.isEmpty)
        (term, tail)
      else
        parsing(tail, currentTerm :+ term)
    case ')' =>
      LifeTree(
        Some(currentTerm(0)),
        currentTerm(1).node,
        Some(currentTerm(2))
      ) -> list.tail
    case 'X' =>
      parsing(list.tail, currentTerm :+ LifeTree(node = On))
    case '.' =>
      parsing(list.tail, currentTerm :+ LifeTree(node = Off))
  }
}

def setFather(term: Term, father: Option[CellTerm] = None): LifeTree = {
  term match {
    case On          => LifeTree(node = On)
    case Off         => LifeTree(node = On)
    case x: LifeTree =>
      x.copy(
        left = x.left.map(setFather(_, Some(x.node))),
        right = x.right.map(setFather(_, Some(x.node))),
        father = father
      )
  }
}

val a =
  parsing(s)._1

setFather(a)
