package functionFractal

import utils.SetInt

/**
  * Created by yujieshui on 2016/3/15.
  */
object TreeLife {


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
          left = Some(currentTerm(0)),
          node = currentTerm(1).node,
          right = Some(currentTerm(2))
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


  def rule7708(father: CellTerm,
               left: CellTerm,
               self: CellTerm,
               right: CellTerm) = {

  }

  def main(args: Array[String]) {
    def readListInt() = io.StdIn.readLine().split(" ").toList.map(_.toInt)
    val ruleNum :: Nil = readListInt()
    val cellTreeString = io.StdIn.readLine().toList.filter(_ != ' ')

    val a = parsing(cellTreeString)


    println(a)
  }

  SetInt(
    """1
      |((. X (. . .)) . (X . (. X X)))
      |
    """.stripMargin)
}
